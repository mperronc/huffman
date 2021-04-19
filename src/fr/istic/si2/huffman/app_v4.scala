/**
 * Matthias PERRONCEL
 * Tudwall CREZE
 * Groupe MA1-A
 */

package fr.istic.si2.huffman

import Encodage._
import Decodage._
import Utils._
import ConstructionCode._
import FonctionsV4._

import scala.io.StdIn._
import java.io.FileNotFoundException


/**
 * Application principale V4 utilisant les fonctions optimisées
 */
object HuffmanApp4 extends App {

  /**
   * Boucle d'interaction principale de l'App V4
   */
  def appV4(): Unit = {
    demanderMode() match {
      case ModeCodage   => modeCodage()
      case ModeDecodage => modeDecodage()
    }

    println("Encore une fois ? [Y/n]")
    val ans = readChar()
    if (ans == 'y' || ans == 'Y')
      appV4()
  }

  /**
   * Demande à l'utilisateur un chemin de fichier et encode son contenu dans un nouveau fichier au chemin
   * donné suivi de ".code"
   */
  def modeCodage(): Unit = {
    val (chemin, texte) = demanderFichier()
    val txtEncode = encodeV4(texte)
    val l1 = texte.length() * 16
    val l2 = txtEncode.length()
    val compr = (l1 - l2) / l1.toFloat

    ecrireFichier(chemin + ".code", txtEncode)

    println("Le texte encodé à été écrit dans le fichier \"" + chemin + ".code\"")
    println("Taille du fichier original : " + l1 + " bits.")
    println("Taille du fichier encodé : " + l2 + " bits.")
    println("Taux de compression : " + "%.2f".format(compr * 100.0) + "%.")
  }

  /**
   * Demande à l'utilisateur un chemin de fichier et decode son contenu dans un nouveau fichier au chemin
   * donné suivi de ".decode"
   */
  def modeDecodage(): Unit = {
    val (chemin, texte) = demanderFichier()
    val txtDecode = decodeV4(texte)

    println("Le contenu décodé est :\n")
    println(txtDecode)
    println()

    ecrireFichier(chemin + ".decode", txtDecode)
    println("Le texte décodé à été écrit dans le fichier \"" + chemin + ".decode\"")
  }

  /**
   * Demande le nom d'un fichier à l'utilisateur et renvoie le contenu
   * de celui-ci. Réitère le demande si le fichier n'est pas trouvé.
   * @return le couple (chemin du fichier, contenu du fichier)
   */
  def demanderFichier(): (String, String) = {
    println("Veuillez entrer un nom de fichier")
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

  /**
   * Demande à l'utilsateur de choisir le mode d'opération de l'application
   * et réitère la demande jusqu'à otenir une réponse valide
   * @return le mode d'operation choisi par l'utilisateur
   */
  def demanderMode(): Mode = {
    println("Quel opération effectuer ?")
    println("1. Encoder un fichier")
    println("2. Decoder un fichier")
    readChar() match {
      case '1' => ModeCodage
      case '2' => ModeDecodage
      case _   => println("Entrée non valide"); demanderMode()
    }
  }

  appV4()
}