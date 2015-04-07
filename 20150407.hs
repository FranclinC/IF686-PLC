-- Aula 04 PLC

--Definir a função abaixo
-- area :: Shape -> Float

data DiasSemana = Segunda Int [String]| Terça Int [String] | Quarta Int [String] | Quinta Int [String] | Sexta Int [String]| Sabado | Domingo

-- Função que recebe um dia da semana e diz se é fim-de-semana
isWeekend :: DiasSemana -> Bool
isWeekend Sabado = True
isWeekend Domingo = True
isWeekend _ = False

-- Função que informa se há ou não aula de PLC naquele dia
isAulaPLC :: DiasSemana -> Bool
isAulaPLC Domingo = False
isAulaPLC Sabado = False
isAulaPLC (Segunda h disc) = elem "PLC" disc
isAulaPLC (Terça h disc) = elem "PLC" disc
isAulaPLC (Quarta h disc) = elem "PLC" disc
isAulaPLC (Quinta h disc) = elem "PLC" disc
isAulaPLC (Sexta h disc) = 	elem "PLC" disc

-- Exercicio Tree

