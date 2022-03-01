import scala.::
import scala.annotation.tailrec

type Grille = List[(Int,Int)]



val liste = List(" XX",
                 "  X",
                 "XXX")

def chainesToGrille(l:List[String]) : Grille = {

  def parcoursString(s: String, x: Int, y: Int, g: Grille): Grille ={
    if (s.nonEmpty) {
      if (s.head == 'X') {
        parcoursString(s.tail, x + 1, y, g ++ List((y,x)))
      }else
        parcoursString(s.tail, x + 1, y, g)
    }else
      g

  }

  def aux(l: List[String], x: Int, y: Int, g:Grille): Grille ={
    if(l != Nil) {
      //print("l pas nil \n")

      aux(l.tail, 0, y + 1, parcoursString(l.head, x, y, g))
    } else {
      //print("l nil")
      g
    }
  }
  aux(l,0,0, Nil)

}

def displayGrille(l: List[(Int,Int)]): Unit ={
  def aux(l: (Int, Int)): Unit = {
    if(l != null){
      print("("+l._1+",")
      print(l._2+")\n")
    }
  }
  if(l!=Nil){
    aux(l.head)
    displayGrille(l.tail)
  } else print("kjehfj")


}

assert(chainesToGrille(liste) == List((0,1), (0,2), (1,2), (2,0), (2,1), (2,2)))

displayGrille(chainesToGrille(liste))