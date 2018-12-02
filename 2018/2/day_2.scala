import scala.io.Source
import scala.collection.mutable.ListBuffer
import scala.util.control.Breaks._


// Read a file in

val lines = Source.fromFile("./input.txt").getLines.toArray

// initialise checksum
var checksum = 0

// function to check for a specific number of repeats in a string
def checkRepeats(a: String, b: Int) : Boolean = {
  var sequenced = a.split("").toSeq
  for (item <- sequenced.distinct) {
    if (sequenced.groupBy(identity).mapValues(_.size)(item) == b) return true
  }
  return false 
}


// 
var i = 0
var doubles = 0
var triples = 0
while (i < lines.length) {
  if (checkRepeats(lines(i), 2)) doubles += 1
  if (checkRepeats(lines(i), 3)) triples += 1
  i += 1
}

checksum = doubles * triples
printf("Checksum: %d \n", checksum)

// function to take a string and return a List of all the single character removed strings

def stringPerms(a: String) : ListBuffer[String] = {
  var returnBuffer = ListBuffer[String]()
  var stringSequence = a.split("")
  var tempSequence = ListBuffer[String]()
  for (i <- 0 to stringSequence.length-1) {
     //var j = -1
     //returnBuffer += stringSequence.filter{_=> j = j+1; j != i}.mkString("")
     tempSequence = stringSequence.to[ListBuffer]
     tempSequence(i) = "!"
     returnBuffer += tempSequence.mkString("")
  }
  return returnBuffer
  
} 

var longList = ListBuffer[String]()

i = 0
while (i < lines.length) {
  longList ++= stringPerms(lines(i))
  printf("%d/%d \n", i, lines.length)
  i += 1
}
  for (item <- longList.distinct) {
    if (longList.groupBy(identity).mapValues(_.size)(item) == 2) printf("Repeated value: %s", item)
  }

