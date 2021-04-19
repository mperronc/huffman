/**
 * Matthias PERRONCEL
 * Tudwall CREZE
 * Groupe MA1-A
 */

package fr.istic.si2.huffman

import fr.istic.si2.huffman.Utils._
import fr.istic.si2.huffman.ConstructionCode._
import fr.istic.si2.huffman.Encodage._
import fr.istic.si2.huffman.Decodage._

object FonctionsV4 {

  /**
   * @param s une chaîne de caractères
   * @return la liste des couples (caractère, fréquence d'apparition),
   *         calculée à partir de s. Chaque élément couple (c, f) est tel que
   *         c est un caractère apparaissant dans s, et f est sa fréquence
   *         d'apparition dans s.
   */
  def analyseFrequencesV4(s: String): List[(Char, Double)] =
    compteOccurencesV4(s.toList).toList.map({ case (c, n) => (c, n / s.length.toDouble) })

  /**
   * @param lc Une liste de caractères
   * @return Un dictionnaire avec pour clés les caractères aparaissant dans lc
   * et pour valeurs le nombre d'occurence de chaque clé
   * @note Utilisation de Map au lieu de List pour obtenir une complexité linéaire
   */
  def compteOccurencesV4(lc: List[Char]): Map[Char, Int] = {
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

  /**
   * @param lb une liste de bits
   * @param h un arbre de Huffman
   * @return la chaîne correspondant au décodage de l, selon h, si elle existe
   * @note Refactorisation en fonction recursive terminale et en évitant de concaténer
   * des longues listes ensemble
   */
  def decodeV4(lb: List[Bit], h: Huffman): Option[String] = {
    def aux(lb: List[Bit], h: Huffman, acc: List[Char]): Option[List[Char]] = {
      lb match {
        case Nil => Some(acc)
        case b :: rem => decodeSymbol(h, lb) match {
          case (Some(c), suffix) => aux(suffix, h, c :: acc)
          case (None, _)         => None
        }
      }
    }
    aux(lb, h, Nil) match {
      case Some(lc) => Some(lc.reverse.mkString(""))
      case None     => None
    }
  }

  /**
   * @param messageEnc une chaîne de 0 et 1 uniquement, contenant la représentation
   *                   d'un arbre de Huffman, puis un message encodé
   * @return le message décodé contenu dans messageEnc, en utilisant le code de huffman
   *         représenté en début de messageEnc
   */
  def decodeV4(messageEnc: String): String = {
    val (h, l) = lireDescription(stringToListBit(messageEnc))
    decodeV4(l, h) match {
      case Some(s) => s
      case None    => "Erreur"
    }
  }

  /**
   * @param lc une liste de caractères
   * @param h un arbre de Huffman
   * @return la séquence de bits correspondants à
   *         l'encodage selon h des éléments de l, s'il a réussi.
   *         Les caractères pour lesquels l'encodage est impossible sont oubliés
   * @note Refactorisation en fonction recursive terminale et en évitant de concaténer
   * des longues listes ensemble
   */
  def encodeListV4(lc: List[Char], h: Huffman): List[Bit] = {
    def aux(lc: List[Char], h: Huffman, acc: List[Bit]): List[Bit] = {
      lc match {
        case Nil => acc
        case c :: rem => {
          encodeSymbol(c, h) match {
            case Some(l) => aux(rem, h, l.reverse ::: acc)
            case None    => aux(rem, h, acc)
          }
        }
      }
    }
    aux(lc, h, Nil).reverse
  }

  /**
   * @param s une chaîne de caractères
   * @param h un arbre de Huffman
   * @return l'encodage de s, selon h, en une liste de bits.
   *         (concaténation de l'encodage de chaque caractère de s selon h)
   */
  def encodeV4(s: String, h: Huffman): List[Bit] = {
    encodeListV4(s.toList, h)
  }

  /**
   * @param message une chaîne de caractères
   * @return la chaîne de 0 et 1, contenant:
   *         - la représentation de l'arbre de huffman construit à partir de message
   *         - puis l'encodage de message en utilisant l'arbre construit à partir de message
   */
  def encodeV4(message: String): String = {
    val arbre = codeHuffman(analyseFrequencesV4(message))
    descriptionHuffman(arbre) + listBitToStringV4(encodeV4(message, arbre))
  }

  /**
   * @param l une liste de bits
   * @return la chaîne de 0 et 1 où chaque bit de l est représenté par 0 ou 1, dans l'ordre
   * @note Refactorisation pour eviter les concaténation de strings
   */
  def listBitToStringV4(l: List[Bit]): String = {
    def aux(l: List[Bit], acc: List[Char]): List[Char] = {
      l match {
        case Nil          => acc
        case One :: rem  => aux(rem, '1' :: acc)
        case Zero :: rem => aux(rem, '0' :: acc)
      }
    }
    aux(l, Nil).reverse.mkString("")
  }
}