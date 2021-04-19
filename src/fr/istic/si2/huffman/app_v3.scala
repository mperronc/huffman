package fr.istic.si2.huffman

import Encodage._
import Decodage._
import Utils._
import ConstructionCode._

import scala.io.StdIn._
import java.io.FileNotFoundException

/**
 * Application principale V3 : avec transmission du code
 */
object HuffmanApp3 extends App {

  @scala.annotation.tailrec /**
   * Boucle d'interaction utilisateur
   */
  def boucleInteraction(): Unit = {

    val (chemin, texte) = demanderFichier()

    //println("Le contenu du fichier est :\n")
    //println(texte + "\n")

    println("Encodage du texte en cours...")
    val texteEncode = encode(texte)
    println("Terminé")


    //println("Le contenu encodé est :\n")
    //println(texteEncode + "\n")

    println("Taille du texte original : " + texte.length() * 16 + " bits. (1 caractère = 16 bits)")
    println("Taille du texte encodé : " + texteEncode.length() + " bits.")

    println("Décodage du texte en cours...")
    val texteDecode = decode(texteEncode)
    println("Termniné")
    //println("Le texte encodé, une fois décodé est :\n")
    //println(texteDecode + "\n")

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