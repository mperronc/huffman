package fr.istic.si2.huffman

import Encodage._
import Decodage._
import Utils._

/**
 * Application principale V0 : arbre de code fixé, encodage/décodage de caractères
 */
object HuffmanApp0 extends App {

  /**
   * Arbre de code utilisé par l'application principale
   */
  val figure1: Huffman =
    Noeud(
      1.0,
      Noeud(
        0.57,
        Feuille(0.25, 'a'), Noeud(
          0.32,
          Feuille(0.18, 'c'), Feuille(0.14, 'd'))),
      Noeud(0.43, Feuille(0.21, 'b'), Noeud(
        0.22,
        Noeud(
          0.13,
          Feuille(0.07, 'f'), Feuille(0.06, 'g')), Feuille(0.09, 'e'))))

  val chars = List('a', 'b', 'c', 'd', 'e', 'f', 'g')

  chars.foreach((c) => {
    val code = encodeSymbol(c, figure1).get
    val decode = decodeSymbolv0(figure1, code).get
    println(c + " " + code.toString() + " " + listBitToString(code) + " " + decode)
  })
}