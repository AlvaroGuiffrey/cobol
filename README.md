# cobol
Sistema de Gestión Comercial

Desarrollado en Cobol, para empresas y sucursales, permitiendo administrar la gestión comercial de las mismas. 
El sistema esta diseñado en módulos: Facturación, cuentas corrientes, contabilidad general, etc.

En cuanto a la programación, para permitir una buena reutilización de código, se escribieron script con código correspondientes a las diferentes secciones y/o divisiones del lenguaje para copiar/incluir dentro del scrip principal ".cbl". Se trató de obtener la mayor abstracción posible.
Tanto el nombre de los script, de los archivos, campos y demás utilizados se difenieron de manera semántica para facilitar su comprensión.
Se utilizó un modelo para posicionamiento en la pantalla mediante el vector TAB-POS() , cuyos datos de seis dígitos permite definir la posición de línea y columna y el largo del campo, facilitando el ordenamiento de las tareas de borrado, limpieza, despliegue y captura en pantalla.  
La técnica utilizada nos permitió desarrollar y mantener los script de manera muy agil.

