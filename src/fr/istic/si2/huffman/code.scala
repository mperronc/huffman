package fr.istic.si2.huffman

import fr.istic.si2.huffman.Utils._


object ConstructionCode {

  /**
   * @param l une liste de couples caractère/fréquence
   * @return la liste des arbres de Huffman réduits à des feuilles,
   *         un pour chaque élément de l
   */
  def initHuffman(l: List[(Char, Double)]): List[Huffman] = l map({case (c, f) => Feuille(f, c)})
  
  /**
   * @param l une liste d'arbres de Huffman
   * @return la liste des éléments de l, classée par ordre croissant des fréquences aux racines
   */
  def triSelonFreq(l: List[Huffman]): List[Huffman] = l sortWith((h1, h2) => extractFreq(h1) < extractFreq(h2))

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
  def fusion(l: List[Huffman]): Huffman =
  {
    l match {
      case n :: Nil => n
      case _        => fusion(uneFusion(l))
    }
  }

  /**
   * @param freqs une liste de couples caractère/fréquence
   * @return l'arbre de code de Huffman correspondant à freqs
   */
  def codeHuffman(freqs: List[(Char, Double)]): Huffman = {
    fusion(initHuffman(freqs))
  }

  /**
   * @param s une chaîne de caractères
   * @return la liste des couples (caractère, fréquence d'apparition),
   *         calculée à partir de s. Chaque élément couple (c, f) est tel que
   *         c est un caractère apparaissant dans s, et f est sa fréquence
   *         d'apparition dans s.
   */
  def analyseFrequences(s: String): List[(Char, Double)] = compteOccurences(s.toList).toList.map({case (c, n) => (c, n / s.length.toDouble)})
  
  /**
   * @param lc Une liste de caractères
   * @return la liste des couples (caractère, nombre d'occurences dans lc)
   */
  def compteOccurences(lc: List[Char]): Map[Char, Int] = {
    def aux(lc: List[Char], acc: Map[Char, Int]): Map[Char, Int] = {
      lc match {
        case Nil => acc
        case c :: rem => {
          val n = acc.getOrElse(c, 0) + 1
          aux(rem, acc + (c -> n))
        }
      }
    }
    aux(lc, Map())
  }
}