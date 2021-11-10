import scala.collection.mutable._
object IBS3_Project_Scala {

var str = "GAGGAGGA"                              //> str  : String = GAGGAGGA
var k = 3                                         //> k  : Int = 3

  // Splitting a string into kmers
  def kmer_decomposition(DNA_string: String, k: Int): MutableList[String] = {
    var kmers = MutableList[String]()
    val num_kmers = (DNA_string.length() - k)
    for (i <- 0 to num_kmers) {
      kmers += DNA_string.slice(i, i + k)
    }
    return kmers
  }                                               //> kmer_decomposition: (DNA_string: String, k: Int)scala.collection.mutable.Mut
                                                  //| ableList[String]

  // Function to sort kmers
  def sorting(kmers: MutableList[String]): MutableList[String] = {
    var lex = kmers
    // for loop which loops through the kmers
    for (i <- 1 to lex.length - 1) {
      for (j <- (i - 1) to 0 by -1) {
        // checks if lex(j) is greater than lex(j+1)
        if (lex(j) > lex(j + 1)) {
          var temp = lex(j + 1)
          lex = lex.updated(j + 1, lex(j))
          lex = lex.updated(j, temp)
        }
      }
    }
    return lex
  }                                               //> sorting: (kmers: scala.collection.mutable.MutableList[String])scala.collecti
                                                  //| on.mutable.MutableList[String]

  def BWT(Input: String): String = {
    var InputSeq = Input + "$" // Add "$" to the end of the string
    var table = MutableList[String]()

    // create a table of right rotated characters of given dna string with $ appended
    for (i <- 0 to InputSeq.length - 1) {
      table += InputSeq.substring(i, InputSeq.length) + InputSeq.substring(0, i)
    }
    table = sorting(table) // sorting the table
    var last_col = ListBuffer[String]()
    var bwt = ""
    for (i <- 0 to table.length - 1) {
      // takes the last character of each of the elements
      bwt += table(i).substring(table(i).length - 1, table(i).length)
    }
    return bwt
  }                                               //> BWT: (Input: String)String
  var BWT_out = BWT(str)                          //> BWT_out  : String = AGGGGG$AA


  def BWT_split(DNA_string: String, k: Int): MutableList[String] = {
  	// generates kmers from input string
    var kmers = kmer_decomposition(DNA_string, k)
    // sorts kmers
    var sorted_kmers = sorting(kmers)
    // performs BWT on each of the kmer
    var BWT_split_out = MutableList[String]()
    for (i <- 0 to sorted_kmers.length - 1) {
      BWT_split_out += BWT(sorted_kmers(i))
    }
    return BWT_split_out
  }                                               //> BWT_split: (DNA_string: String, k: Int)scala.collection.mutable.MutableList
                                                  //| [String]
  var t1 = System.nanoTime                        //> t1  : Long = 372510762638800
  var BWT_split_out = BWT_split(str, k)           //> BWT_split_out  : scala.collection.mutable.MutableList[String] = MutableList
                                                  //| (G$GA, G$GA, GGA$, GGA$, AGG$, AGG$)
  val duration1 = (System.nanoTime - t1) / 1e9d   //> duration1  : Double = 0.0578757
	print(BWT_split_out)                      //> MutableList(G$GA, G$GA, GGA$, GGA$, AGG$, AGG$)




  def IBWT(BWT_out: String): String = {
    var table = ListBuffer[String]() // Make empty table
    for (i <- 0 to BWT_out.length - 1) { // Add a column
      table += ""
    }
    for (i <- 0 to BWT_out.length - 1) {
      for (j <- 0 to BWT_out.length - 1) {
      // Concatenating given BWT string w sorted words
        table(j) = BWT_out(j) + table(j)
      }
      table = table.sorted
    }
    var inverse_bwt = ""
    for (i <- 0 to table.length - 1) {
      if (table(i).endsWith("$")) {
        inverse_bwt = inverse_bwt + table(i).substring(0, table(i).length - 1) // Returning w/o $
      }
    }
    return inverse_bwt
  }                                               //> IBWT: (BWT_out: String)String

var IBWT_out = IBWT(BWT_out)                      //> IBWT_out  : String = GAGGAGGA

  def IBWT_split(BWT_out_split: MutableList[String]): MutableList[String] = {
    var IBWT_kmers = MutableList[String]()
    for (i <- 0 to BWT_out_split.length - 1) {
      IBWT_kmers += IBWT(BWT_out_split(i))
    }
    return IBWT_kmers
  }                                               //> IBWT_split: (BWT_out_split: scala.collection.mutable.MutableList[String])sc
                                                  //| ala.collection.mutable.MutableList[String]
  var t2 = System.nanoTime                        //> t2  : Long = 372510828487300
  var IBWT_out_split = IBWT_split(BWT_split_out)  //> IBWT_out_split  : scala.collection.mutable.MutableList[String] = MutableLis
                                                  //| t(AGG, AGG, GAG, GAG, GGA, GGA)
  var duration2 = (System.nanoTime - t2) / 1e9d   //> duration2  : Double = 0.0030038

}