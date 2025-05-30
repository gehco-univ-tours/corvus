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
    timestamp TIMESTAMPTZ NOT NULL,
    sensor_id INTEGER NOT NULL REFERENCES sensor(id),
    value DOUBLE PRECISION,
    value_corr DOUBLE PRECISION,
    CONSTRAINT measurement_pkey PRIMARY KEY (timestamp, sensor_id)
);

SELECT create_hypertable('measurement', by_range('timestamp'));

CREATE UNIQUE INDEX idx_measurement_timestamp_sensor_id ON measurement(timestamp, sensor_id);

CREATE INDEX idx_measurement_sensor_id ON measurement(sensor_id);

CREATE TABLE correction (
    id SERIAL PRIMARY KEY,
    sensor_id INTEGER NOT NULL REFERENCES sensor(id),
    author_id INTEGER NOT NULL REFERENCES author(id),
    timestamp_start TIMESTAMPTZ NOT NULL,
    timestamp_end TIMESTAMPTZ NOT NULL,
    correction_type INTEGER NOT NULL REFERENCES correction_type(id),
    value DOUBLE PRECISION NOT NULL,
    comment TEXT
);

INSERT INTO correction_type (name) VALUES ('Offset'), ('Drift'), ('Delete'), ('Interpolation');

CREATE TABLE field (
    timestamp TIMESTAMPTZ NOT NULL,
    author_id INTEGER NOT NULL REFERENCES author(id),
    station_id INTEGER NOT NULL REFERENCES station(id),
    comment TEXT,
    CONSTRAINT field_pkey PRIMARY KEY (timestamp, station_id)
);

-- Add view (optional)

CREATE VIEW measurement_data AS (
    SELECT
        station.code AS station,
        parameter.name AS parameter,
        measurement.timestamp AS timestamp,
        measurement.value AS value,
        measurement.value_corr AS value_corr
    FROM measurement
    JOIN sensor ON measurement.sensor_id = sensor.id
    JOIN parameter ON sensor.parameter_id = parameter.id
    JOIN station ON sensor.station_id = station.id)
    ORDER BY timestamp ASC;
