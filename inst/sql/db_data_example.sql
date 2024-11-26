-- Add data examples to empty database

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