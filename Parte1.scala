object Ejercicio1 {

    val duplicar: Int => Int = x => x * 2
    val cuadrado: Int => Int = x => x * x
    val esPar: Int => Boolean = x => x % 2 == 0

    def ejecutar(): Unit = {
        println(s"duplicar(5) = ${duplicar(5)}")   
        println(s"cuadrado(4) = ${cuadrado(4)}")   
        println(s"esPar(8) = ${esPar(8)}")         
        println(s"esPar(7) = ${esPar(7)}")          
    }
}


object Ejercicio2 {

    val cubo: Int => Int = x => x * x * x
    val promedio: (Double, Double) => Double = (a, b) => (a + b) / 2
    val mayor: (Int, Int) => Int = (a, b) => if a > b then a else b
    val esMultiploDeTres: Int => Boolean = x => x % 3 == 0

    def ejecutar(): Unit = {
        println(s"cubo(3) = ${cubo(3)}")                      
        println(s"promedio(4, 9) = ${promedio(4, 9)}")         
        println(s"mayor(7, 12) = ${mayor(7, 12)}")             
        println(s"esMultiploDeTres(9) = ${esMultiploDeTres(9)}") 
        println(s"esMultiploDeTres(10) = ${esMultiploDeTres(10)}") 
    }
}

object Ejercicio3 {

    def transformar(numero: Int, operacion: Int => Int): Int = {
        operacion(numero)
    }

    def ejecutar(): Unit = {
        println(s"duplicar: ${transformar(5, x => x * 2)}")   
        println(s"cuadrado: ${transformar(5, x => x * x)}")   
        println(s"sumar 10: ${transformar(5, x => x + 10)}")  
        println(s"negativo: ${transformar(5, x => -x)}")      
    }
}

object Ejercicio4 {

    def calcular(
        a: Double,
        b: Double,
        operacion: (Double, Double) => Double
    ): Double = {
        operacion(a, b)
    }

    def ejecutar(): Unit = {
        println(s"suma: ${calcular(10, 4, (x, y) => x + y)}")        
        println(s"resta: ${calcular(10, 4, (x, y) => x - y)}")            
        println(s"multiplicacion: ${calcular(10, 4, (x, y) => x * y)}")   
        println(s"division: ${calcular(10, 4, (x, y) => x / y)}")          
        println(s"promedio: ${calcular(10, 4, (x, y) => (x + y) / 2)}")   
    }
}

object Ejercicio5 {

    def crearMultiplicador(factor: Int): Int => Int = {
        x => x * factor
    }

    val duplicar = crearMultiplicador(2)
    val triplicar = crearMultiplicador(3)
    val porDiez = crearMultiplicador(10)

    def ejecutar(): Unit = {
        println(s"duplicar(7) = ${duplicar(7)}")   
        println(s"triplicar(7) = ${triplicar(7)}") 
        println(s"porDiez(7) = ${porDiez(7)}")     
    }
}

object Ejercicio6 {

    def aplicarDescuento(precio: Double, porcentaje: Double): Double = {
        precio - (precio * porcentaje / 100)
    }

    val descuento10: Double => Double = precio => aplicarDescuento(precio, 10)
    val descuento20: Double => Double = precio => aplicarDescuento(precio, 20)
    val descuento50: Double => Double = precio => aplicarDescuento(precio, 50)

    def ejecutar(): Unit = {
        println(s"descuento10(500) = ${descuento10(500)}")  
        println(s"descuento20(500) = ${descuento20(500)}")  
        println(s"descuento50(500) = ${descuento50(500)}")  
    }
}

object Ejercicio7 {

    def elevar(exponente: Int)(base: Int): Int = {
        if exponente == 0 then 1
        else base * elevar(exponente - 1)(base)
    }

    val cuadrado: Int => Int = elevar(2)
    val cubo: Int => Int = elevar(3)
    val cuartaPotencia: Int => Int = elevar(4)

    def ejecutar(): Unit = {
        println(s"cuadrado(5) = ${cuadrado(5)}")              
        println(s"cubo(3) = ${cubo(3)}")                        
        println(s"cuartaPotencia(2) = ${cuartaPotencia(2)}")    
    }
}

object Ejercicio8 {

    val limpiar: String => String = texto => texto.trim
    val convertirMinusculas: String => String = texto => texto.toLowerCase
    val longitud: String => Int = texto => texto.length
    val esLongitudValida: Int => Boolean = n => n >= 5

    val validarTexto: String => Boolean =
        limpiar
            .andThen(convertirMinusculas)
            .andThen(longitud)
            .andThen(esLongitudValida)

    val validarTexto2: String => Boolean =
        esLongitudValida
            .compose(longitud)
            .compose(convertirMinusculas)
            .compose(limpiar)

    def ejecutar(): Unit = {
        println(s"validarTexto(\"  Scala  \") = ${validarTexto("  Scala  ")}") 
        println(s"validarTexto(\"  Hola  \") = ${validarTexto("  Hola  ")}")   
        println(s"validarTexto2(\"  Scala  \") = ${validarTexto2("  Scala  ")}")
        println(s"validarTexto2(\"  Hola  \") = ${validarTexto2("  Hola  ")}") 
    }
}

object Ejercicio9 {

    def sumar(desde: Int, hasta: Int, transformar: Int => Int): Int = {
        if desde > hasta then 0
        else transformar(desde) + sumar(desde + 1, hasta, transformar)
    }

    def producto(desde: Int, hasta: Int, transformar: Int => Int): Int = {
        if desde > hasta then 1
        else transformar(desde) * producto(desde + 1, hasta, transformar)
    }

    def ejecutar(): Unit = {
        val n = 4
        println(s"1 + 2 + ... + n = ${sumar(1, n, x => x)}")            
        println(s"1^2 + 2^2 + ... + n^2 = ${sumar(1, n, x => x * x)}")    
        println(s"1^3 + 2^3 + ... + n^3 = ${sumar(1, n, x => x * x * x)}") 

        println(s"sumar(1, 4, x => x) = ${sumar(1, 4, x => x)}")          
        println(s"sumar(1, 3, x => x * x) = ${sumar(1, 3, x => x * x)}")  
        println(s"producto(1, 4, x => x) = ${producto(1, 4, x => x)}")   
    }
}

object Ejercicio10 {

    def transformarTexto(texto: String, transformacion: Char => Char): String = {
        if texto.isEmpty then ""
        else s"${transformacion(texto.head)}${transformarTexto(texto.tail, transformacion)}"
    }

    def buscarCaracter(texto: String, condicion: Char => Boolean): Boolean = {
        if texto.isEmpty then false
        else if condicion(texto.head) then true
        else buscarCaracter(texto.tail, condicion)
    }

    def ejecutar(): Unit = {
        println(transformarTexto("scala", c => c.toUpper))     
        println(buscarCaracter("Scala2026", c => c.isDigit))       
        println(buscarCaracter("Scala", c => c == 'z'))          
    }
}
object Parte1 {

    def main(args: Array[String]): Unit = {
        println("Ejercicio 1")
        Ejercicio1.ejecutar()
        println("Ejercicio 2")
        Ejercicio2.ejecutar()
        println("Ejercicio 3")
        Ejercicio3.ejecutar()
        println("Ejercicio 4")
        Ejercicio4.ejecutar()
        println("Ejercicio 5")
        Ejercicio5.ejecutar()
        println("Ejercicio 6")
        Ejercicio6.ejecutar()
        println("Ejercicio 7")
        Ejercicio7.ejecutar()
        println("Ejercicio 8")
        Ejercicio8.ejecutar()
        println("Ejercicio 9")
        Ejercicio9.ejecutar()
        println("Ejercicio 10")
        Ejercicio10.ejecutar()
    }
}