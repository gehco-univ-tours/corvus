-- Add data examples to empty database

INSERT INTO author (name) VALUES ('Louis Manière');

INSERT INTO STATION (code, name, latitude, longitude) VALUES
('MTJ', 'Montjean', 47.39431260145717, -0.8595363116063837),
('WIL', 'Wilson', 47.39926065412902, 0.685079293234682);

INSERT INTO device_model (name, brand, type) VALUES
('Turbidity NTU', 'Aqualabo/PONSEL', 'Nephelometry Turbidity sensor'),
('PLS', 'OTT', 'Level pressure sensor'),
('Leveline Gauge mini', 'NKE Aquaread', 'Level pressure sensor'),
('Wimo Plus temperature', 'NKE', 'Multiparameter sonde temperature'),
('Wimo Plus conductivity', 'NKE', 'Multiparameter sonde conductivity'),
('Wimo Plus turbidity', 'NKE', 'Multiparameter sonde turbidity'),
('Wimo Plus oxygen', 'NKE', 'Multiparameter sonde oxygen'),
('Wimo Plus pH', 'NKE', 'Multiparameter sonde pH'),
('AquaTroll 500 level', 'In-Situ', 'Multiparameter sonde level');

INSERT INTO device (serial_num, device_model_id) VALUES
('SN-PNEPB-2353', 1),
('654321', 2),
('Wimo-level-001', 3),
('Wimo-temperature-001', 4),
('Wimo-conductivity-001', 5),
('Wimo-turbidity-001', 6),
('Wimo-oxygen-001', 7),
('Wimo-pH-001', 8),
('15586', 9)
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
('pH', 'pH', 0.1, 'pH', 8),
('Conductivity', 'µS/cm', 0.5, '%', 9),
('Oxygen concentration', 'mg/L', 0.1, 'mg/L', 9),
('pH', 'pH', 0.1, 'pH', 9)
;

INSERT INTO sensor (station_id, parameter_id, name) VALUES
(1, 1, 'MTJ Turbidity'),
(1, 2, 'MTJ Level'),
(1, 3, 'MTJ Temperature'),
(1, 9, 'MTJ conductivity'),
(1, 9, 'MTJ Oxygen'),
(1, 9, 'MTJ pH'),
(2, 4, 'WIL Level'),
(2, 5, 'WIL Temperature'),
(2, 6, 'WIL Conductivity'),
(2, 7, 'WIL Turbidity'),
(2, 8, 'WIL Oxygen'),
(2, 9, 'WIL pH')
;
