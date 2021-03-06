/**
 * Matthias PERRONCEL
 * Tudwall CREZE
 * Groupe MA1-A
 */

package fr.istic.si2.huffman

import Utils._
import ConstructionCode._

object Encodage {

  /**
   * @param c un caractère
   * @param h un arbre de Huffman
   * @return l'encodage de c, selon h (si c est bien présent dans h)
   */
  def encodeSymbol(c: Char, h: Huffman): Option[List[Bit]] = {
    h match {
      case Noeud(_, h1, h2) => {
        encodeSymbol(c, h1) match {
          case Some(l) => Some(Zero :: l)
          case None => encodeSymbol(c, h2) match {
            case Some(l) => Some(One :: l)
            case None    => None
          }
        }
      }
      case Feuille(_, fc) => if (fc == c) Some(Nil) else None
    }
  }

  /**
   * @param lc une liste de caractères
   * @param h un arbre de Huffman
   * @return la séquence de bits correspondants à
   *         l'encodage selon h des éléments de l, s'il a réussi.
   *         Les caractères pour lesquels l'encodage est impossible sont oubliés
   */
  def encodeList(lc: List[Char], h: Huffman): List[Bit] = {
    lc match {
      case Nil => Nil
      case c :: rem => {
        encodeSymbol(c, h) match {
          case Some(lb) => lb ++ encodeList(rem, h)
          case None    => encodeList(rem, h)
        }
      }
    }
  }

  /**
   * @param s une chaîne de caractères
   * @param h un arbre de Huffman
   * @return l'encodage de s, selon h, en une liste de bits.
   *         (concaténation de l'encodage de chaque caractère de s selon h)
   */
  def encode(s: String, h: Huffman): List[Bit] = encodeList(s.toList, h)

  /**
   * @param h un arbre de Huffman
   * @return une chaîne de 0 et 1 uniquement représentant l'arbre h (voir partie 1.3 de l'énoncé)
   *         Les caractères encodables avec h sont représentés dans leur encodage binaire 16 bits.
   */
  def descriptionHuffman(h: Huffman): String = {
    h match {
      case Noeud(_, h1, h2) => "1" + descriptionHuffman(h1) + descriptionHuffman(h2)
      case Feuille(_, c)    => "0" + vers16Bits(c.toString())
    }
  }

  /**
   * @param message une chaîne de caractères
   * @return la chaîne de 0 et 1, contenant:
   *         - la représentation de l'arbre de huffman construit à partir de message
   *         - puis l'encodage de message en utilisant l'arbre construit à partir de message
   */
  def encode(message: String): String = {
    val arbre = codeHuffman(analyseFrequences(message))
    descriptionHuffman(arbre) + listBitToString(encode(message, arbre))
  }
}