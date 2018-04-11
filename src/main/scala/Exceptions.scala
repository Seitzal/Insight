package seitzal.scalastat

class ColNotFoundException(cname : String) extends Exception("No variable named " + cname + " exists in dataset."){}
class NotNumericException(cname : String) extends Exception(cname + " is not numeric."){}
class InvalidColException(cname : String) extends Exception(cname + " is invalid or corrupted."){}

class NameNotUniqueException() extends Exception("Datasets may not contain multiple rows with the same variable name"){}