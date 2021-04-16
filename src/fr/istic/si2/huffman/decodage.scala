package fr.istic.si2.huffman

import Utils._

object Decodage {

  /**
   * @param h un arbre de Huffman
   * @param l une liste de bits
   * @return caractère correspondant au décodage de l selon h
   *          si l est un chemin valide de h
   */
  def decodeSymbolv0(h: Huffman, l: List[Bit]): Option[Char] = {
    (h, l) match {
      case (Feuille(_, c), Nil)             => Some(c)
      case (Noeud(_, h1, h2), Zero :: tail) => decodeSymbolv0(h1, tail)
      case (Noeud(_, h1, h2), One :: tail)  => decodeSymbolv0(h2, tail)
      case _                                => None
    }
  }

  /**
   * @param h un arbre de Huffman
   * @param l une liste de bits
   * @return un tuple de taille 2
   *         - première composante : caractère correspondant au décodage selon h d'un préfixe de l
   *         - deuxième composante : la liste des bits restant à décoder
   */
  def decodeSymbol(h: Huffman, l: List[Bit]): (Option[Char], List[Bit]) = {
    (h, l) match {
      case (Feuille(_, c), l)               => (Some(c), l)
      case (Noeud(_, _, _), Nil)            => (None, l)
      case (Noeud(_, h1, h2), Zero :: tail) => decodeSymbol(h1, tail)
      case (Noeud(_, h1, h2), One :: tail)  => decodeSymbol(h2, tail)
    }
  }

  /**
   * @param l une liste de bits
   * @param h un arbre de Huffman
   * @return la chaîne correspondant au décodage de l, selon h, si elle existe
   */
  def decode(l: List[Bit], h: Huffman): Option[String] = {
    l match {
      case Nil => Some("")
      case b :: tail => {
        decodeSymbol(h, l) match {
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
   * @param l une liste de bits décrivant la représentation binaire d'un arbre de Huffman
   * @return Si la liste de bits a pu etre lue, un tuple de taille 2 comprenant :
   *         - l'arbre de code de Huffman reconstruit à partir du début de l
   *         - le reste de la liste l, après la représentation de l'arbre de Huffman
   */
  def lireDescription(l: List[Bit]): Option[(Huffman, List[Bit])] = {
    // Construit l'arbre a partir de la description et compte le nombre de bits utilisés
    def aux(l: List[Bit]): Option[(Huffman, Int)] = {
      l match {
        case Nil         => None
        case Zero :: rem => Some((Feuille(0, toChar(listBitToString(rem.take(16)))), 17))
        case One :: rem => {
          aux(rem) match {
            case Some((h1, n1)) => aux(rem.drop(n1)) match {
              case Some((h2, n2)) => Some(Noeud(0, h1, h2), 1 + n1 + n2)
              case None           => None
            }
            case None => None
          }
        }
      }
    }
    aux(l) match {
      case Some((h, n)) => Some((h, l.drop(n)))
      case None         => None
    }
  }

  /**
   * @param messageEnc une chaîne de 0 et 1 uniquement, contenant la représentation
   *                   d'un arbre de Huffman, puis un message encodé
   * @return le message décodé contenu dans messageEnc, en utilisant le code de huffman
   *         représenté en début de messageEnc
   */
  def decode(messageEnc: String): String = {
    lireDescription(stringToListBit(messageEnc)) match {
      case Some((h, l)) => decode(l, h) match {
        case Some(s) => s
        case None    => "Erreur"
      }
      case None => "Erreur"
    }
  }
}