import scala.io.Source

// Read a file in

val lines = Source.fromFile("./input_test.txt").getLines.toArray

val claims = Array.ofDim[Boolean](lines.size +1)

// create a cloth 2d array
val cloth = Array.ofDim[Int](1000,1000) 

// create a function to parse a line
def parseLine(a: String) : Tuple4[Int, Int, Int, Int] = {
    var spaceSeparated = a.split(" ").toSeq
    var offsets = spaceSeparated(2).dropRight(1).split(",")
    var dimensions = spaceSeparated(3).split("x")
    return (offsets(0).toInt, offsets(1).toInt, dimensions(0).toInt, dimensions(1).toInt)
}


// function that takes four integers and draws on the cloth
def drawCloth(x : Int,  y : Int , width : Int, height: Int) : Unit = {
    for (i <- x to x+width-1 ){
        for (j <- y to y + height-1) {
            cloth(i)(j) += 1
        }
    }
}

def checkCollision(x : Int,  y : Int , width : Int, height: Int) : Boolean = {
    for (i <- x to x+width-1 ){
        for (j <- y to y + height-1) {
            if (cloth(i)(j) > 2) return true
        }
    }
    return false
}

// loop to iterate over the input

for (line <- lines ) {
    // draw on the cloth for each line
    var(x, y, width, height) = parseLine(line)
    drawCloth(x, y, width, height)

}

// count each square in the cloth with a claims > 2

var count = 0
for (i <- 0 to cloth.size -1){ 
    for (j <- 0 to cloth(0).size -1 ) {
        if (cloth(i)(j) > 1) count += 1
        //printf("%d", cloth(i)(j))
    }
    //printf("\n")
}

printf("Output: %d", count)

// do it again, if any rectangle only draws on itself this time, it's not a collision.

var claimnumber = 1 
for (line <- lines ) {
    // draw on the cloth for each line
    var(x, y, width, height) = parseLine(line)
    drawCloth(x, y, width, height)
    claims(claimnumber) = checkCollision(x, y, width, height)
    claimnumber += 1
}

claimnumber = 1 
for (claim <- claims) {
    if (!claim) printf("Claim Number %d", claimnumber)
    claimnumber += 1
}
