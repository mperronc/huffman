/**
 * Matthias PERRONCEL
 * Tudwall CREZE
 * Groupe MA1-A
 */

package fr.istic.si2.huffman

import Encodage._
import Decodage._
import ConstructionCode._
import Utils._
import scala.io.StdIn._

/**
 * Application principale V1 : arbre de code fixé
 */
object HuffmanApp1 extends App {

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
          Feuille(0.07, 'f'), Feuille(0.06, 'g')),
        Feuille(0.09, 'e'))))

  /**
   * Boucle d'interaction utilisateur
   */
  def appV1(): Unit = {
    println("Chaîne à encoder ?")
    val chaine = readLine()

    println("Chaîne encodée standard :")
    val chaine16Bits = vers16Bits(chaine)
    println(chaine16Bits)
    println("taille (nb Bits) : " + chaine16Bits.length())

    println("Chaîne encodée Huffman :")
    val chaineEncodee = encode(chaine, figure1)
    val chaineEncodeeString = listBitToString(chaineEncodee)
    println(chaineEncodeeString)
    println("taille (nb Bits) : " + chaineEncodeeString.length())

    println("Chaîne décodée Huffman :")
    val chaineDecodee = decode(chaineEncodee, figure1)
    chaineDecodee match {
      case None => println("Erreur")
      case Some(s) => {
        println(s)
        if (s.length() != chaine.length())
          println("Erreur ou caractère(s) non encodable(s)")
      }
    }

    println("Encore ? [Y/n]")
    val ans = readChar()
    if (ans == 'y' || ans == 'Y')
      appV1()
  }

  appV1()
}