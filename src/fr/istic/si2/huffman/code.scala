/**
 * Matthias PERRONCEL
 * Tudwall CREZE
 * Groupe MA1-A
 */

package fr.istic.si2.huffman

import Utils._

object ConstructionCode {

  /**
   * @param lcd une liste de couples caractère/fréquence
   * @return la liste des arbres de Huffman réduits à des feuilles,
   *         un pour chaque élément de l
   */
  def initHuffman(lcd: List[(Char, Double)]): List[Huffman] = {
    lcd match {
      case Nil           => Nil
      case (c, f) :: rem => Feuille(f, c) :: initHuffman(rem)
    }
  }

  /**
   * @param lh une liste d'arbres de Huffman
   * @return la liste des éléments de l, classée par ordre croissant des fréquences aux racines
   */
  def triSelonFreq(lh: List[Huffman]): List[Huffman] = {
    def insert(h: Huffman, lh: List[Huffman]): List[Huffman] = {
      lh match {
        case Nil => h :: Nil
        case x :: rem => {
          if (extractFreq(h) < extractFreq(x)) h :: x :: rem
          else x :: insert(h, rem)
        }
      }
    }
    lh match {
      case Nil      => Nil
      case x :: rem => insert(x, triSelonFreq(rem))
    }
  }

  /**
   * @param lh une liste d'arbres de Huffman, de longueur au moins 2
   * @return la liste obtenue après avoir fusionné les 2 arbres de l de fréquences minimales
   */
  def uneFusion(lh: List[Huffman]): List[Huffman] = {
    val h1 :: h2 :: rem = triSelonFreq(lh)
    Noeud(extractFreq(h1) + extractFreq(h2), h1, h2) :: rem
  }

  /**
   * @param lh une liste NON VIDE d'arbres de Huffman.
   * @return l'arbre de Huffman obtenu en fusionnant successivement,
   *         et 2 par 2, les arbres de l de fréquences minimales
   */
  def fusion(lh: List[Huffman]): Huffman = {
    lh match {
      case h :: Nil => h
      case _        => fusion(uneFusion(lh))
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
    def aux(lci: List[(Char, Int)]): List[(Char, Double)] = {
      lci match {
        case Nil           => Nil
        case (c, n) :: rem => (c, n / s.length.toDouble) :: aux(rem)
      }
    }
    aux(compteOccurences(s.toList))
  }

  /**
   * @param lc Une liste de caractères
   * @return la liste des couples (caractère présent dans lc, nombre d'occurences dans lc)
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