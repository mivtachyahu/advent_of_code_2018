import scala.io.Source
import scala.collection.mutable.ListBuffer
import scala.util.control.Breaks._


// Read a file in

val lines = Source.fromFile("./input.txt").getLines.toArray

// Sum variable for the final frequency
var sum=0

// define a function that checks for duplicates in a list
def containsDuplicates(a: ListBuffer[Int]) : Boolean = {
   if (a.distinct.size != a.size) return true else return false 
}

// while loop to create sum and populate history
var i=0
while (i < lines.length) {
  sum += lines(i).toInt
  i += 1
}

var history = ListBuffer[Int](0)
var repeated = 0
i = 0
var frequency = 0 
var found = false
while (found == false) {
  frequency += lines(i).toInt
  history += frequency
  i += 1
  if (containsDuplicates(history)) {
    found = true
    repeated = frequency
  }
  if (i == lines.length) { 
    i=0
    printf(".")
  }
}


printf("Output: %d \n", sum)
printf("First repeated: %d \n", repeated)
