# TRABAJO FINAL DEL MÁSTER

## Predicción de potenciales compradores en un ecommerce

## Autor

Jose Rodriguez Guzman

## Informacion adicional

* Universidad Oberta de Catalunya [Universitat Oberta of Catalunya.](http://www.uoc.edu/portal/ca/index.html)
* Master in Data Science
* Tutor: Santiago Rojo Muñoz
* Profesor: Albert Solé Ribalta

## Resumen

Este proyecto se enfocó en la predicción de potenciales compradores en un ecommerce con sede en Colombia. Es decir, con base en los datos 
que se han almacenado de los visitantes de una web de venta online, establecer su nivel potencial de compra. Inicialmente se contó con dos 
bases de datos, una con características de los usuarios y otra con los consumos realizados por ellos. Este segundo archivo, fue transformado 
en otro que resumiera para cada usuario sus consumos y así, tener en un repositorio final toda la información junta. Uno de los retos que se 
presentaron fue trabajar con datos desbalanceados, es decir, que menos del 1% de los usuarios eran clientes versus un 99% de no clientes, es 
decir, se contaba con poca información sobre los usuarios convertidos, lo cual era un problema para poder entrenar los modelos. El segundo 
desafío enfrentado fue la gran cantidad de nulos que tenían muchas de las variables, por lo que se tomó la decisión de crear dos grupos, uno 
donde se tuviera la información completa y el otro con los que no se tuviera todo. En ambos subconjuntos de datos se utilizaron técnicas
estadísticas como el análisis de multicolinearidad y pruebas de chi-cuadrado, así como el algoritmo Boruta para la elección de las variables 
que finalmente se utilizaron en los modelos. Al igual que las investigaciones realizadas sobre proyectos similares, se emplearon los algoritmos 
de aprendizaje automático: árboles de decisiones, bosques aleatorios y regresión logística. El modelo de árboles de decisiones fue el que mostró 
los mejores resultados en ambos escenarios, logrando valores de AUC de 0.999, una precisión global de 0.997 y una especificidad del 100%. 
La única diferencia en ambos fue la proporción de usuarios que fueron predichos como clientes correctamente, donde el grupo 1 obtuvo un 81,35%,
mientras que el del grupo 2 fue del 65%. Finalmente, se indica que la variable del indicador de que un usuario fue al botón de pago es la 
característica de mayor peso para la predicción de los potenciales compradores el ecommerce.

## Código

## Licencia

El contenido de este proyecto esta licencia bajo la [Creative Commons Attribution-NonCommercial-ShareAlike 4.0 International](https://creativecommons.org/licenses/by-nc-sa/4.0/), 
y el código fuente usadao para mostrar este contenido esta licenciado bajo la  [MIT license](http://opensource.org/licenses/mit-license.php).
