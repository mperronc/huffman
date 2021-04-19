package fr.istic.si2.huffman

import fr.istic.si2.huffman.Utils._

object ConstructionCode {

  /**
   * @param l une liste de couples caractère/fréquence
   * @return la liste des arbres de Huffman réduits à des feuilles,
   *         un pour chaque élément de l
   */
  def initHuffman(l: List[(Char, Double)]): List[Huffman] = {
    l match {
      case Nil            => Nil
      case (c, f) :: tail => Feuille(f, c) :: initHuffman(tail)
    }
  }

  /**
   * @param l une liste d'arbres de Huffman
   * @return la liste des éléments de l, classée par ordre croissant des fréquences aux racines
   */
  def triSelonFreq(l: List[Huffman]): List[Huffman] = {
    def insert(h: Huffman, l: List[Huffman]): List[Huffman] = {
      l match {
        case Nil => h :: Nil
        case head :: tail if extractFreq(h) < extractFreq(head) => h :: head :: tail
        case head :: tail => head :: insert(h, tail)
      }
    }
    l match {
      case Nil       => Nil
      case h :: tail => insert(h, triSelonFreq(tail))
    }
  }

  /**
   * @param l une liste d'arbres de Huffman, de longueur au moins 2
   * @return la liste obtenue après avoir fusionné les 2 arbres de l de fréquences minimales
   */
  def uneFusion(l: List[Huffman]): List[Huffman] = {
    val h1 :: h2 :: rem = triSelonFreq(l)
    Noeud(extractFreq(h1) + extractFreq(h2), h1, h2) :: rem
  }

  /**
   * @param l une liste NON VIDE d'arbres de Huffman.
   * @return l'arbre de Huffman obtenu en fusionnant successivement,
   *         et 2 par 2, les arbres de l de fréquences minimales
   */
  def fusion(l: List[Huffman]): Huffman = {
    l match {
      case n :: Nil => n
      case _        => fusion(uneFusion(l))
    }
  }

  /**
   * @param freqs une liste de couples caractère/fréquence
   * @return l'arbre de code de Huffman correspondant à freqs
   */
  def codeHuffman(freqs: List[(Char, Double)]): Huffman = fusion(initHuffman(freqs))

  /**
   * @param s une chaîne de caractères
   * @return la liste des couples (caractère, fréquence d'apparition),
   *         calculée à partir de s. Chaque élément couple (c, f) est tel que
   *         c est un caractère apparaissant dans s, et f est sa fréquence
   *         d'apparition dans s.
   */
  def analyseFrequences(s: String): List[(Char, Double)] = {
    def aux(l: List[(Char, Int)]): List[(Char, Double)] = {
      l match {
        case Nil           => Nil
        case (c, n) :: rem => (c, n / s.length.toDouble) :: aux(rem)
      }
    }
    aux(compteOccurences(s.toList))
  }

  /**
   * @param lc Une liste de caractères
   * @return la liste des couples (caractère, nombre d'occurences dans lc)
   */
  def compteOccurences(lc: List[Char]): List[(Char, Int)] = {
    def aux(lc: List[Char], acc: List[(Char, Int)]): List[(Char, Int)] = {
      lc match {
        case Nil => acc
        case c :: rem => {
          if (apparait(c, acc))
            aux(rem, ajouteUn(c, acc))
          else
            aux(rem, (c, 1) :: acc)
        }
      }
    }
    aux(lc, List())
  }

  /**
   * @param c Un caractère
   * @param lc Une liste de caractères
   * @return si le caractère c apparait dans la liste lc
   */
  def apparait(c: Char, lc: List[(Char, Int)]): Boolean = {
    lc match {
      case Nil => false
      case (x, n) :: rem => {
        if (c == x) true
        else apparait(c, rem)
      }
    }
  }

  /**
   * @param c Un caractère
   * @param compte Une liste de couple (Char, Int)
   * @return La liste compte tel que le deuxième membre de chaque
   *         couple contenant c soit incrémenté de 1
   */
  def ajouteUn(c: Char, compte: List[(Char, Int)]): List[(Char, Int)] = {
    compte match {
      case Nil => Nil
      case (x, n) :: rem => {
        if (c == x) (x, n + 1) :: ajouteUn(c, rem)
        else (x, n) :: ajouteUn(c, rem)
      }
    }
  }

}