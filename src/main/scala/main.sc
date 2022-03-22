import scala.::
import scala.annotation.tailrec

type Grille = List[(Int,Int)]



val liste1 = List(" XX",
                 "  X",
                 "XXX")



val liste = List("  X  "," XXX ","XX XX"," XXX ","  X  ")

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

assert(chainesToGrille(liste1) == List((0,1), (0,2), (1,2), (2,0), (2,1), (2,2)))


/** Question 2 */

def afficherGrille(li: List[(Int,Int)]): Unit ={
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

      if(li.contains((y, minX))) print(" X ")
      else print(" - ")
      parcoursLigne(minX +1, maxX, y)

    }
  }
  def aux(min: (Int, Int), max: (Int, Int)): Unit = {

    if(min._1 <= max._1) {

      parcoursLigne(min._2, max._2, min._1)

      print("\n")
      aux((min._1+1,min._2), max)
    }
  }

  if(li!=Nil){
    aux(minimum(li), maximum(li))
  } else print("grille vide !")

}

afficherGrille(chainesToGrille(liste))



/** Question 3 */

def voisines8(l:Int, c:Int):List[(Int, Int)] = {
  (l+1,c)::(l-1,c)::(l,c+1)::(l,c-1)::(l+1,c+1)::(l-1,c-1)::(l+1,c-1)::(l-1,c+1)::Nil
}

assert(voisines8(1,1) == List((2,1), (0,1), (1,2), (1,0), (2,2), (0,0), (2,0), (0,2)))



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
      parcours8(voisines8(grille.head._1, grille.head._2),0) match {
        case 2 | 3 => aux(grille.tail, grilleR ++ List((grille.head._1,grille.head._2)))
        case _ => aux(grille.tail , grilleR)
      }
    }
  }
  aux(g, Nil)
}

afficherGrille(survivantes(chainesToGrille(liste)))


/** Question 5 */


def candidates(g:Grille): Grille = {
  def parcours8(v : List[(Int,Int)], res: Grille): Grille = {
    if(v == Nil){
      res
    } else {
      if(g.contains(v.head)){
        parcours8(v.tail, res)
      } else {
        parcours8(v.tail, res++List((v.head._1,v.head._2)))
      }
    }
  }
  def aux(grille: Grille, grilleR: Grille) : Grille = {
    if(grille == Nil){
      grilleR
    } else {
      aux(grille.tail, parcours8(voisines8(grille.head._1, grille.head._2),grilleR))
    }
  }
  aux(g, Nil)
}

afficherGrille(candidates(chainesToGrille(liste)))

/** Question 6 */

def naissances(g: Grille) : Grille = {
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
    if (grille == Nil) {
      grilleR
    } else {
      parcours8(voisines8(grille.head._1, grille.head._2), 0) match {
        case 3 => aux(grille.tail, grilleR ++ List((grille.head._1, grille.head._2)))
        case _ => aux(grille.tail, grilleR)
      }
    }
  }
  aux(candidates(g), Nil)
}

afficherGrille(naissances(chainesToGrille(liste)))

/** Question 7 */

def jeuDeLaVie(init: Grille, n:Int) : Unit ={
  if (n >= 0) {
    afficherGrille(init)
    print('\n')
    jeuDeLaVie(survivantes(init)++naissances(init),n-1)
  }
}

jeuDeLaVie(chainesToGrille(liste),5)


/** Question 8 */

def voisines4(l:Int, c:Int):List[(Int, Int)] = {
  (l+1,c)::(l-1,c)::(l,c+1)::(l,c-1)::Nil
}

/** Question 9 */

def naitJDLV(nbVoisines:Int) : Boolean = (nbVoisines == 3)

def survieJDLV(nbVoisines:Int) : Boolean = (nbVoisines == 2 || nbVoisines == 3)

def naitFred(nbVoisines:Int) : Boolean = (0 == nbVoisines%2)

def survieFred(nbVoisines:Int) : Boolean = (0 == nbVoisines%2)

/** Question 10 */

def survivantesG(g:Grille, voisines: (Int, Int)=>List[(Int, Int)] , survie: Int => Boolean ):Grille = {
  def parcoursG(v : List[(Int,Int)], res: Int): Int = {
    if(v == Nil){
      res
    } else {
      if(g.contains(v.head)){
        parcoursG(v.tail, res +1)
      } else {
        parcoursG(v.tail, res)
      }
    }
  }
  def aux(grille: Grille, grilleR: Grille): Grille = {
    if (grille == Nil){
      grilleR
    } else {
        if (survie(parcoursG(voisines(grille.head._1, grille.head._2),0))) {
          aux(grille.tail, grilleR ++ List((grille.head._1, grille.head._2)))
        } else {
          aux(grille.tail , grilleR)
      }
    }
  }
  aux(g, Nil)
}




def candidatesG(g:Grille, voisines: (Int, Int)=>List[(Int, Int)]): Grille = {
  def parcoursG(v : List[(Int,Int)], res: Grille): Grille = {
    if(v == Nil){
      res
    } else {
      if(g.contains(v.head)){
        parcoursG(v.tail, res)
      } else {
        parcoursG(v.tail, res++List((v.head._1,v.head._2)))
      }
    }
  }
  def aux(grille: Grille, grilleR: Grille) : Grille = {
    if(grille == Nil){
      grilleR
    } else {
      aux(grille.tail, parcoursG(voisines(grille.head._1, grille.head._2),grilleR))
    }
  }
  aux(g, Nil)
}



def naissancesG(g: Grille, voisines: (Int, Int)=>List[(Int, Int)] , nait: Int => Boolean ) : Grille = {
  def parcoursG(v : List[(Int,Int)], res: Int): Int = {
    if(v == Nil){
      res
    } else {
      if(g.contains(v.head)){
        parcoursG(v.tail, res +1)
      } else {
        parcoursG(v.tail, res)
      }
    }
  }
  def aux(grille: Grille, grilleR: Grille): Grille = {
    if (grille == Nil) {
      grilleR
    } else {
      if (nait(parcoursG(voisines(grille.head._1, grille.head._2), 0))) {
        aux(grille.tail, grilleR ++ List((grille.head._1, grille.head._2)))
      } else {
        aux(grille.tail, grilleR)
      }
    }
  }
  aux(candidatesG(g, voisines), Nil)
}


/** Question 11 */

def moteur(init: Grille, n:Int, voisines: (Int, Int)=>List[(Int, Int)] , nait: Int => Boolean, survie: Int => Boolean ) : Unit ={
  if (n >= 0) {
    afficherGrille(init)
    print('\n')
    moteur(survivantesG(init, voisines, survie)++naissancesG(init, voisines, nait),n-1, voisines, nait, survie)
  }
}

/** Question 12 */

moteur(chainesToGrille(liste), 4, voisines8, survieJDLV, naitJDLV)
moteur(chainesToGrille(liste), 4, voisines4, survieFred, naitFred)

/** Question 13 */

def voisines4Fred(l:Int, c:Int):List[(Int, Int)] = {
  (l+1,c+1)::(l-1,c-1)::(l+1,c-1)::(l-1,c+1)::Nil
}

moteur(chainesToGrille(liste), 4, voisines4Fred, survieFred, naitFred)