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
import java.io.FileNotFoundException

/**
 * Application principale V2 : avec construction du code
 */
object HuffmanApp2 extends App {

  @scala.annotation.tailrec /**
   * Boucle d'interaction utilisateur
   */
  def boucleInteraction(): Unit = {

    val (chemin, texte) = demanderFichier()

    println("Le contenu du fichier est :\n")
    println(texte + "\n")

    val lfreqs = analyseFrequences(texte)
    val arbre = codeHuffman(lfreqs)

    val texteEncode = encode(texte, arbre)
    val texte16bits = vers16Bits(texte)

    println("Taille du texte original : " + texte16bits.length() + " bits.")
    println("Taille du texte encodé : " + texteEncode.size + " bits.")

    ecrireFichier(chemin + ".code", listBitToString(texteEncode))
    println("Le texte encodé à été écrit dans le fichier " + chemin + ".code")

    println("Encore ? [Y/n]")
    val ans = readChar()
    if (ans == 'y' || ans == 'Y')
      boucleInteraction()
  }

  /**
   * Demande le nom d'un fichier à l'utilisateur et renvoie le contenu
   * de celui-ci. Réitère le demande si le fichier n'est pas trouvé.
   * @return le couple (chemin du fichier, contenu du fichier)
   */
  @scala.annotation.tailrec
  def demanderFichier(): (String, String) = {
    println("Veuillez entrer le nom du fichier à encoder")
    val s = readLine()
    try {
      (s, lireFichier(s))
    } catch {
      case e: FileNotFoundException => {
        println("Fichier non trouvé")
        demanderFichier()
      }
    }
  }

  boucleInteraction()

}