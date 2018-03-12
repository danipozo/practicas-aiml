{-# LANGUAGE OverloadedStrings #-}

import DSL

questions = [ "COMO ^ [APR SUP]+ LA PARTE PRACTICA",
             "COMO ^ [EVA APR]+ EN LA CONVOCATORIA ORDINARIA",
             "HA+ EXAMEN DE PRACTICAS EN LA CONVOCATORIA EXTRAORDINARIA",
             "^ NO PUEDO ASISTIR A CLASE REGULARMENTE",
             "^ NOTA MINIMA PARA APROBAR",
             "^ PRACTICAS ^ OBLIGATORIAS",
             "^ VALE CADA PRACTICA ^ RESPECTO ^ NOTA FINAL",
             "^ VALE ^ PARTICIP+  ^ RESPECTO ^ LA NOTA FINAL",
             "^ PUNTOS POR PARTICIPACION",
             "^ TRABAJOS ^ INDIVIDUAL+",
             "^ HACER (LOS) TRABAJOS EN GRUPO",
             "^ ENTREGAR ^ RELACIONES DE PROBLEMAS",
             "^ VAL+ EL PRIMER EXAMEN DE TEORIA",
             "(NO) PUEDO RECUPERAR UNA PRACTICA O EXAMEN",
             "^ COPIO (DE OTRO ESTUDIANTE)",
             "^ DEJO COPIAR (DE OTRO ESTUDIANTE)",
             "^ SUSPENDO ^ CONVOCATORIA ORDINARIA",
             "CUANTO VALE+ (LA) PRACTICA Y (LA) TEORIA",
             "^ HORARIO DE TUTORIAS DEL PROFESOR DE PRACTICAS",
             "^ APROB+ (LA PARTE) PRACTICA DE LA ASIGNATURA ^ PRESENT+ UNA PRACTICA",
             "^ NOTA MINIMA ^ CADA PARTE (PARA HACER MEDIA)",
             "(QUE) PUNTUACION +TENGO EN LA PRACTICA 1 ^ NIVEL 3 Y NO LO SUPERO",
             "^ PRACTICA 1 SIN HACER ^ NIVEL 0",
             "^ (NO) PUEDO ASISTIR A ^ PRACTICA+ ^ DEFENSA DE (MI) (PRIMERA) PRACTICA (1)",
             "CUANT+ [PRUEBAS EXAMENES] TEORIC+ TENDRE ^",
             "^ VALE ^ PRIMER+ [PRUEBA EXAMEN] ^ TEOR+",
             "^ CONSISTE ^ EXAMEN ^ EJERCICIOS",
             "^ NO ESTOY SEGURO ^ SUPERAR ^ NIVEL 3 ^ PRACTICA 1",
             "^ PODRIA HACER LA PRACTICA 1 EN ^ LENGUAJE",
             "^ PODRIA USAR UN AGENTE CONVERSACIONAL ^ EN MI LUGAR A HACER LA DEFENSA DE PRACTICAS"
  ]

answers = ["Para la parte de prácticas se tendrá en cuenta la asistencia y participación en las clases (esta parte constituye el 10% de la nota de prácticas), y adicionalmente se realizarán tres prácticas y un examen de problemas. Este último se realizará conjuntamente con el examen final de teoría. La nota será la media de estas cuatro pruebas (esta parte constituye el 90% de la nota de prácticas).",
           "Para la parte de teoría durante el curso se realizarán dos pruebas. La nota de teoría será la media sobre las dos pruebas.",
           "No, hay un examen escrito único con preguntas teóricas y prácticas, que garantizarán que el alumno ha adquirido la totalidad de las competencias descritas en la guía docente (por imposible que parezca garantizarlo mediante un examen).",
           "Puedes acogerte a la modalidad de evaluación única final.",
           "La nota mínima para aprobar es de cinco.",
	   "Cada práctica individualmente no, tener una nota media mínima de tres sí.",
	   "El 11,25 %.",
	   "Estadísticamente nada. De acuerdo a la guía docente, un 5 %.",
	   "¿Por participación? Solo soy un bot, no te pases conmigo.",
	   "Sí.",
	   "No. Tienes que enfrentarte a AIML sol@. Lo siento.",
	   "No.",
	   "El 25 % de la nota final.",
	   "Diría que no.",
	   "Si te copias de otro estudiante, has conseguido llegar a uno de los pocos casos en los que se aplica la Normativa de Evaluación y Calificación de los Estudiantes de la Universidad de Granada, y automáticamente suspendes la asignatura.",
	   "No tengo ni idea de lo que significa \"dejarse copiar\". Si lo entendemos como facilitar tus prácticas a otra persona, directa o indirectamente, con o sin intención de que las copie, entonces no tienes derecho a hacer lo que quieras con tu trabajo y suspenderás igual que si te hubieras copiado.",
	   "Vas a la extraordinaria.",
	   "50-50.",
	   "Lo tienes que buscar en internet.",
	   "Sí.",
	   "3",
	   "6",
	   "Poder, puedes.",
	   "Debes justificarlo adecuadamente.",
	   "Dos",
	   "Esta pregunta está repetida. El 25 %",
	   "En problemas de búsqueda.",
           "Eso depende del grado de creencia en la ocurrencia del suceso de superarla.",
           "No. Tienes que usar AIML, y enfrentarte a un intérprete que no te señala errores, no toma la entrada como el resto de intérpretes, a veces no detecta la modificación de un fichero de texto, no acepta caracteres no ASCII; y a programar en un dialecto de XML en el que no tienes las construcciones usuales de lenguaje de programación. Es por tu bien.",
           "Bien podrías."
	   ]

for = flip map
doc = for (zip questions answers) $ (\(q,a) ->
        cat [
              pat  [q],
              temp [a]
            ])

main :: IO ()
main = do putStrLn $ renderDocument doc
