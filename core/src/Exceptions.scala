package eu.seitzal.insight

class ColNotFoundException(cname : String) 
  extends Exception("No variable named " + cname + " exists in dataset")

class RowNotFoundException(index : Int)
  extends Exception("No row exists at index " + index)

class NotNumericException(cname : String) 
  extends Exception(cname + " is not numeric")

class InvalidSeriesException(cname : String) 
  extends Exception(cname + " is invalid or corrupted")

class ZeroCountException(valdesc : String) 
  extends Exception(valdesc + " must be positive")

class NameNotUniqueException() 
  extends Exception("Data frame may not contain multiple rows with the same " +
  "variable name")

class MissingValueException(operation : String) 
  extends Exception("Data must not contain missing values in order to " +
  "calculate " + operation)

class ColLengthException(operation : String) 
  extends Exception("Data series must contain the same number of values in " +
  "order to calculate " + operation)
