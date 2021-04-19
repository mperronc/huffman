/**
 * Matthias PERRONCEL
 * Tudwall CREZE
 * Groupe MA1-A
 */

package fr.istic.si2.huffman

import Utils._

object Decodage {

  /**
   * @param h un arbre de Huffman
   * @param lb une liste de bits
   * @return caractère correspondant au décodage de l selon h
   *          si l est un chemin valide de h
   */
  def decodeSymbolv0(h: Huffman, lb: List[Bit]): Option[Char] = {
    (h, lb) match {
      case (Feuille(_, c), Nil)             => Some(c)
      case (Noeud(_, h1, h2), Zero :: rem) => decodeSymbolv0(h1, rem)
      case (Noeud(_, h1, h2), One :: rem)  => decodeSymbolv0(h2, rem)
      case _                                => None
    }
  }

  /**
   * @param h un arbre de Huffman
   * @param lb une liste de bits
   * @return un tuple de taille 2
   *         - première composante : caractère correspondant au décodage selon h d'un préfixe de l
   *         - deuxième composante : la liste des bits restant à décoder
   */
  def decodeSymbol(h: Huffman, lb: List[Bit]): (Option[Char], List[Bit]) = {
    (h, lb) match {
      case (Feuille(_, c), l)               => (Some(c), l)
      case (Noeud(_, _, _), Nil)            => (None, lb)
      case (Noeud(_, h1, h2), Zero :: rem) => decodeSymbol(h1, rem)
      case (Noeud(_, h1, h2), One :: rem)  => decodeSymbol(h2, rem)
    }
  }

  /**
   * @param lb une liste de bits
   * @param h un arbre de Huffman
   * @return la chaîne correspondant au décodage de l, selon h, si elle existe
   */
  def decode(lb: List[Bit], h: Huffman): Option[String] = {
    lb match {
      case Nil => Some("")
      case b :: rem => {
        decodeSymbol(h, lb) match {
          case (Some(c), suffix) => {
            decode(suffix, h) match {
              case Some(s) => Some(c + s)
              case None    => None
            }
          }
          case (None, suffix) => None
        }
      }
    }
  }

  /**
   * @param lb une liste de bits décrivant la représentation binaire d'un arbre de Huffman
   * @return Si la liste de bits a pu etre lue, un tuple de taille 2 comprenant :
   *         - l'arbre de code de Huffman reconstruit à partir du début de l
   *         - le reste de la liste l, après la représentation de l'arbre de Huffman
   */
  def lireDescription(lb: List[Bit]): (Huffman, List[Bit]) = {
    lb match {
      case Zero :: rem => {
        val c = toChar(listBitToString(take(16, rem)))
        (Feuille(0, c), drop(17, lb))
      }
      case One :: rem => {
        val (f1, rest1) = lireDescription(rem)
        val (f2, rest2) = lireDescription(rest1)
        (Noeud(0, f1, f2), rest2)
      }
      case _ => throw new Exception("Description illisible")
    }
  }

  /**
   * @param messageEnc une chaîne de 0 et 1 uniquement, contenant la représentation
   *                   d'un arbre de Huffman, puis un message encodé
   * @return le message décodé contenu dans messageEnc, en utilisant le code de huffman
   *         représenté en début de messageEnc
   */
  def decode(messageEnc: String): String = {
    val (h, l) = lireDescription(stringToListBit(messageEnc))
    decode(l, h) match {
      case Some(s) => s
      case None    => "Erreur"
    }
  }
}