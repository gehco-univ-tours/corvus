CREATE TABLE author (
    id SERIAL PRIMARY KEY,
    name VARCHAR(255) NOT NULL UNIQUE
);

CREATE TABLE station (
    id SERIAL PRIMARY KEY,
    code VARCHAR(255),
    name VARCHAR(255),
    latitude DOUBLE PRECISION,
    longitude DOUBLE PRECISION
);

CREATE TABLE device_model (
    id SERIAL PRIMARY KEY,
    name VARCHAR(255),
    brand VARCHAR(255),
    type VARCHAR(255)
);

CREATE TABLE device (
    id SERIAL PRIMARY KEY,
    serial_num VARCHAR(255),
    device_model_id INTEGER NOT NULL REFERENCES device_model(id)
);

CREATE TABLE parameter (
    id SERIAL PRIMARY KEY,
    name VARCHAR(255),
    unit VARCHAR(255),
    accuracy DOUBLE PRECISION,
    accuracy_unit VARCHAR(255),
    device_id INTEGER NOT NULL REFERENCES device(id)
);

CREATE TABLE sensor (
    id SERIAL PRIMARY KEY,
    station_id INTEGER NOT NULL REFERENCES station(id),
    parameter_id INTEGER NOT NULL REFERENCES parameter(id),
    name VARCHAR(255),
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
    CONSTRAINT measurement_pkey UNIQUE (timestamp, sensor_id)
);

SELECT create_hypertable('measurement', 'timestamp');

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

-- Add data examples (optional)

INSERT INTO author (name) VALUES ('Louis Manière');

INSERT INTO STATION (code, name, latitude, longitude) VALUES
('BE', 'Beaulieu', 47.15169291, 0.76110491),
('PI', 'Picarderie', 47.15380250, 0.74298863),
('GB', 'Grand Bray', 47.14270218, 0.77490453),
('CY', 'Conteraye', 47.15639589, 0.77015020),
('BP', 'Brépinière', 47.13605000, 0.75261000),
('MA', 'Masniers', 47.14011794, 0.76818525),
('FCD', 'Fosse aux chats drain', 47.14383308, 0.75827271),
('FCR', 'Fosse aux chats rigole', 47.14385134, 0.75820409);

INSERT INTO device_model (name, brand, type) VALUES
('Turbidity NTU', 'PONSEL', 'Nephelometriy Turbidity sensor'),
('PLS', 'OTT', 'Level pressure sensor'),
('Leveline Gauge mini', 'NKE Aquaread', 'Level pressure sensor'),
('Wimo Plus temperature', 'NKE', 'Multiparameter sonde temperature'),
('Wimo Plus conductivity', 'NKE', 'Multiparameter sonde conductivity'),
('Wimo Plus turbidity', 'NKE', 'Multiparameter sonde turbidity'),
('Wimo Plus oxygen', 'NKE', 'Multiparameter sonde oxygen'),
('Wimo Plus pH', 'NKE', 'Multiparameter sonde pH')
;

INSERT INTO device (serial_num, device_model_id) VALUES
('SN-PNEPB-2353', 1),
('654321', 2),
('Wimo-level-001', 3),
('Wimo-temperature-001', 4),
('Wimo-conductivity-001', 5),
('Wimo-turbidity-001', 6),
('Wimo-oxygen-001', 7),
('Wimo-pH-001', 8)
;

INSERT INTO parameter (name, unit, accuracy, accuracy_unit, device_id) VALUES
('Turbidity', 'NTU', 5, '%', 1),
('Level', 'cm', 0.05, '%', 2),
('Temperature', '°C', 0.5, '°C', 2),
('Level', 'cm', 0.1, '%', 3),
('Temperature', '°C', 0.02, '°C', 4),
('Conductivity', '0.5', 2, '%', 5),
('Turbidity', 'FNU', 0.5, '%', 6),
('Oxygen concentration', 'mg/L', 0.1, 'mg/L', 7),
('Oxygen saturation', '%', 0.1, '%', 7),
('pH', 'pH', 0.1, 'pH', 8)
;

INSERT INTO sensor (station_id, parameter_id, name) VALUES
(1, 1, 'BE Turbidity'),
(1, 2, 'BE Level'),
(1, 3, 'BE Temperature'),
(3, 4, 'GB Level'),
(3, 5, 'GB Temperature'),
(3, 6, 'GB Conductivity'),
(3, 7, 'GB Turbidity'),
(3, 8, 'GB Oxygen concentration'),
(3, 9, 'GB Oxygen saturation'),
(3, 10, 'GB pH')
;

