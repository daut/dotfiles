---
description: Query Prometheus metrics from the current project's Grafana dashboard
agent: build
subtask: true
---

Query Prometheus metrics via the Grafana HTTP API and produce a health report. The service and queries are derived from the project's Grafana dashboard YAML.

## Arguments

- `$1` = environment (default: "prod"). Valid values depend on the dashboard but typically: dev, qa, uat, prod

## Step 1: Discover the dashboard

Look for a Grafana dashboard YAML file in the current project directory:
1. Search for `grafana/Dashboard/*.yaml` or `grafana/**/*.yaml` files
2. If multiple dashboards exist, pick the first one (or ask the user)
3. If no dashboard YAML is found, tell the user and stop

## Step 2: Parse the dashboard YAML

Read the dashboard YAML and extract:

1. **App name**: from `spec.templating.list[]` where `name: "app"` -- use its `query` or `current.value` field
2. **Datasource UID**: from `spec.templating.list[]` where `type: "adhoc"` and `datasource.type: "prometheus"` -- use its `datasource.uid`. Alternatively, take it from any panel's `datasource.uid` where `datasource.type: "prometheus"`
3. **Valid environments**: from `spec.templating.list[]` where `name: "environment"` -- use its `options[].value` list
4. **All PromQL expressions**: from each panel's `targets[].expr` field. These contain template variables like `$environment` and `$app`

Validate that the requested environment (`$1`) is in the valid environments list.

## Step 3: Query metrics

Use curl to query the Grafana datasource proxy API. The connection details are:

- Grafana URL: `$GRAFANA_URL` environment variable
- Auth token: `$GRAFANA_TOKEN` environment variable
- Datasource UID: parsed from the dashboard YAML in Step 2

The query endpoint is:
```
GET $GRAFANA_URL/api/datasources/proxy/uid/<datasource-uid>/api/v1/query?query=<url-encoded-promql>
```

For each PromQL expression found in the dashboard:
1. Replace `$environment` with the requested environment
2. Replace `$app` with the app name from the dashboard
3. Replace any other template variables with their `current.value` from the dashboard's templating list
4. URL-encode the expression
5. Execute the query with `curl -s -H "Authorization: Bearer $GRAFANA_TOKEN"`

Parse results with python3. Run independent curl commands in parallel where possible.

## Step 4: Organize results by panel

Group the query results by their panel title (from `spec.panels[].title`). The dashboard organizes panels into rows (collapsed groups) with their own titles. Use both the row title and panel title for context.

For each panel, report:
- The panel title
- The values returned, formatted according to the panel's `unit` field (e.g. `decbytes` = bytes, `percentunit` = percentage, `s` = seconds, `reqps` = requests/sec, `ms` = milliseconds, `cps` = counts/sec)
- Use the `legendFormat` from each target to label the series (replace `{{label}}` with actual label values)

## Step 5: Produce the health report

Present the results as a formatted health report with:
1. A header with: service name, environment, timestamp, and overall status (HEALTHY / DEGRADED / UNHEALTHY)
2. Each dashboard row as a section header (e.g. "JVM Metrics", "HTTP Metrics")
3. Each panel as a sub-section with values in a table or key-value list
4. Flag any concerning values with warnings based on panel thresholds (from `fieldConfig.defaults.thresholds.steps[]`)
5. A "Concerns" section at the bottom listing any issues found (or "None" if all healthy)

Overall status rules:
- UNHEALTHY: any instance down (`up` metric = 0), 5xx error rate >0.1 req/s, memory usage >90% of max, blocked threads >5
- DEGRADED: 4xx error rate >1 req/s, memory usage >80% of max, avg latency >500ms, GC overhead >5%, error log rate >0
- HEALTHY: everything within normal ranges
