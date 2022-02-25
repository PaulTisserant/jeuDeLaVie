type Grille = List[(Int,Int)]

def chainesToGrille(l:List[String]) : Grille = {
  def aux(l:List): Unit ={

    aux(x+1)
    aux(y+1)
  }
  aux(l,0,0);
}