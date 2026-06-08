ALTER DATABASE corvus SET TIME ZONE 'UTC';

CREATE TABLE author (
    id SERIAL PRIMARY KEY,
    code VARCHAR(255) NOT NULL UNIQUE,
    name VARCHAR(255) NOT NULL UNIQUE
);

CREATE TABLE station (
    id SERIAL PRIMARY KEY,
    code VARCHAR(255) NOT NULL UNIQUE,
    name VARCHAR(255) NOT NULL UNIQUE,
    latitude DOUBLE PRECISION  NOT NULL,
    longitude DOUBLE PRECISION  NOT NULL
);

CREATE TABLE parameter (
    id SERIAL PRIMARY KEY,
    name VARCHAR(255) NOT NULL UNIQUE,
    code VARCHAR(255) NOT NULL UNIQUE,
    unit VARCHAR(255)
);

CREATE TABLE sensor (
    id SERIAL PRIMARY KEY,
    station_id INTEGER NOT NULL REFERENCES station(id),
    parameter_id INTEGER NOT NULL REFERENCES parameter(id),
    name VARCHAR(255) NOT NULL,
    CONSTRAINT sensor_uniq UNIQUE (station_id, parameter_id)
);

CREATE TABLE correction_type (
    id SERIAL PRIMARY KEY,
    name VARCHAR(255) NOT NULL UNIQUE
);

CREATE TABLE measurement (
    ts TIMESTAMPTZ NOT NULL,
    sensor_id INTEGER NOT NULL REFERENCES sensor(id),
    value DOUBLE PRECISION,
    CONSTRAINT measurement_pkey PRIMARY KEY (ts, sensor_id)
);


CREATE TABLE measurement_corr (
    ts TIMESTAMPTZ NOT NULL,
    sensor_id INTEGER NOT NULL REFERENCES sensor(id),
    value DOUBLE PRECISION,
    CONSTRAINT measurement_corr_pkey PRIMARY KEY (ts, sensor_id)
);

CREATE TABLE measurement_filter (
    ts TIMESTAMPTZ NOT NULL,
    sensor_id INTEGER NOT NULL REFERENCES sensor(id),
    value DOUBLE PRECISION,
    CONSTRAINT measurement_filter_pkey PRIMARY KEY (ts, sensor_id)
);

SELECT create_hypertable('measurement', by_range('ts'));
CREATE UNIQUE INDEX idx_measurement_ts_sensor_id ON measurement(ts, sensor_id);
CREATE INDEX idx_measurement_sensor_id ON measurement(sensor_id);

SELECT create_hypertable('measurement_corr', by_range('ts'));
CREATE UNIQUE INDEX idx_measurement_corr_ts_sensor_id ON measurement_corr(ts, sensor_id);
CREATE INDEX idx_measurement_corr_sensor_id ON measurement_corr(sensor_id);

SELECT create_hypertable('measurement_filter', by_range('ts'));
CREATE UNIQUE INDEX idx_measurement_filter_ts_sensor_id ON measurement_filter(ts, sensor_id);
CREATE INDEX idx_measurement_filter_sensor_id ON measurement_filter(sensor_id);

CREATE TABLE correction (
    id SERIAL PRIMARY KEY,
    ts_corr TIMESTAMPTZ NOT NULL,
    sensor_id INTEGER NOT NULL REFERENCES sensor(id),
    author_id INTEGER NOT NULL REFERENCES author(id),
    ts_start TIMESTAMPTZ NOT NULL,
    ts_end TIMESTAMPTZ NOT NULL,
    correction_type INTEGER NOT NULL REFERENCES correction_type(id),
    value DOUBLE PRECISION NOT NULL,
    comment TEXT
);

INSERT INTO correction_type (name) VALUES ('Offset'), ('Drift'), ('Delete'), ('Interpolation');

CREATE TABLE field (
    ts TIMESTAMPTZ NOT NULL,
    author_id INTEGER NOT NULL REFERENCES author(id),
    station_id INTEGER NOT NULL REFERENCES station(id),
    comment TEXT,
    CONSTRAINT field_pkey PRIMARY KEY (ts, station_id)
);


-- Add view (optional)

-- hourly materialized view
CREATE MATERIALIZED VIEW measurement_hourly
WITH (timescaledb.continuous) AS
SELECT
	time_bucket(INTERVAL '1 hour', ts) AS ts,
	sensor_id,
	AVG(value) AS value
FROM measurement
GROUP BY sensor_id, time_bucket(INTERVAL '1 hour', ts);

-- Refresh the materialized view every 1 hour
SELECT add_continuous_aggregate_policy('measurement_hourly',
  start_offset => NULL,
  end_offset => INTERVAL '1 h',
  schedule_interval => INTERVAL '1 h');

-- daily materialized view
CREATE MATERIALIZED VIEW measurement_daily
WITH (timescaledb.continuous) AS
SELECT
	time_bucket(INTERVAL '1 day', ts) AS ts,
	sensor_id,
	AVG(value) AS value
FROM measurement
GROUP BY sensor_id, time_bucket(INTERVAL '1 day', ts);

-- Refresh the materialized view every 1 day
SELECT add_continuous_aggregate_policy('measurement_daily',
  start_offset => NULL,
  end_offset => INTERVAL '1 day',
  schedule_interval => INTERVAL '1 day');

-- standard view
CREATE VIEW measurement_data AS (
    SELECT
        station.code AS station,
        parameter.name AS parameter,
        measurement.ts AS ts,
        measurement.value AS value
    FROM measurement
    JOIN sensor ON measurement.sensor_id = sensor.id
    JOIN parameter ON sensor.parameter_id = parameter.id
    JOIN station ON sensor.station_id = station.id)
    ORDER BY ts ASC;
