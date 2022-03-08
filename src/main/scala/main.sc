import scala.::
import scala.annotation.tailrec

type Grille = List[(Int,Int)]



val liste = List(" XX",
                 "  X",
                 "XXX")

/** Question 1 */

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

assert(chainesToGrille(liste) == List((0,1), (0,2), (1,2), (2,0), (2,1), (2,2)))


/** Question 2 */

def displayGrille(li: List[(Int,Int)]): Unit ={
  def minimum (list: List[(Int,Int)]): (Int, Int) ={
    def min (l: List[(Int,Int)], mini: (Int,Int)): (Int, Int) ={
      if(l == Nil){
        mini;
      } else {
        if (mini._1 > l.head._1){
          if(mini._2 > l.head._2){
            min(l.tail, (l.head._1,l.head._2));
          } else {
            min(l.tail, (l.head._1, mini._2));
          }
        } else {
          if(mini._2 > l.head._2){
            min(l.tail, (mini._1,l.head._2));
          } else {
            min(l.tail, mini);
          }
        }
      }
    }
    min(list, (999,999));
  }
  def maximum (list: List[(Int,Int)]): (Int,Int) ={
    def max (l: List[(Int,Int)], maxi: (Int,Int)): (Int,Int) ={
      if(l == Nil){
        maxi;
      } else {
        if (maxi._1 < l.head._1){
          if(maxi._2 < l.head._2){
            max(l.tail, (l.head._1,l.head._2));
          } else {
            max(l.tail, (l.head._1, maxi._2));
          }
        } else {
          if(maxi._2 < l.head._2){
            max(l.tail, (maxi._1,l.head._2));
          } else {
            max(l.tail, maxi);
          }
        }
      }
    }
    max(list, (-999,-999));
  }
  def parcoursLigne(minX: Int, maxX: Int, y: Int): Unit ={
    if(minX <= maxX){

      if(li.contains((y, minX))) print("X ")
      else print("  ")
      parcoursLigne(minX +1, maxX, y)

    }
  }
  def aux(min: (Int, Int), max: (Int, Int)): Unit = {

    if(min._2 <= max._2) {

      parcoursLigne(min._1, max._1, min._2)

      print("\n")
      aux((min._1,min._2+1), max)
    }
  }

  if(li!=Nil){
    aux(minimum(li), maximum(li))
  } else print("grille vide !")

}

displayGrille(chainesToGrille(liste))



/** Question 3 */

def voisines8(l:Int, c:Int):List[(Int, Int)] = {
  (l+1,c)::(l-1,c)::(l,c+1)::(l,c-1)::(l+1,c+1)::(l-1,c-1)::(l+1,c-1)::(l-1,c+1)::Nil
}

/** Question 4 */

def survivantes(g:Grille):Grille = {
  def parcours8(v : List[(Int,Int)], res: Int): Int = {
    if(v == Nil){
      res
    } else {
      if(g.contains(v.head)){
        parcours8(v.tail, res +1)
      } else {
        parcours8(v.tail, res)
      }
    }
  }
  def aux(grille: Grille, grilleR: Grille): Grille = {
    if (grille == Nil){
      grilleR
    } else {
      if(parcours8(voisines8(grille.head._1, grille.head._2),0) <= 1){
        aux(grille.tail, grilleR)
      } else {
        //TODO
      }
    }

  }
}





