/**
 * Matthias PERRONCEL
 * Tudwall CREZE
 * Groupe MA1-A
 */

package fr.istic.si2.huffman

import scala.io.Source
import java.io.{ File, PrintWriter }
import fr.istic.si2._
import org.junit.Assert._

object Utils {

  /**
   * @tparam T Un type quelconque
   * @param n Un entier positif ou nul
   * @param l Une liste
   * @return Les n premiers elements de la liste, ou toute la liste si la liste contient
   * moins de n élements
   */
  def take[T](n: Int, l: List[T]): List[T] = {
    (n, l) match {
      case (0, _) | (_, Nil) => Nil
      case (_, x :: rem)     => x :: take(n - 1, rem)
    }
  }

  /**
   * @tparam T Un type quelconque
   * @param n Un entier positif ou nul
   * @param l Une liste
   * @return La liste sans ses n premiers elements, ou la liste vide si n contient
   * moins de n elements.
   */
  def drop[T](n: Int, l: List[T]): List[T] = {
    (n, l) match {
      case (0, _) | (_, Nil) => l
      case (_, x :: rem)     => drop(n - 1, rem)
    }
  }

  /**
   * @param lb une liste de bits
   * @return la chaîne de 0 et 1 où chaque bit de l est représenté par 0 ou 1, dans l'ordre
   */
  def listBitToString(lb: List[Bit]): String = {
    def aux(lb: List[Bit], acc: String): String = {
      lb match {
        case Nil          => acc
        case One :: rem  => aux(rem, acc + "1")
        case Zero :: rem => aux(rem, acc + "0")
      }
    }
    aux(lb, "")
  }

  /**
   * @param s une chaîne de caractères
   * @return la chaîne de 0 et 1 représentant chaque caractère
   *         de s par son encodage sur 16 bits
   */
  def vers16Bits(s: String): String = {
    s.toList.map(c => String.format("%16s", c.toBinaryString).replace(' ', '0')).foldLeft("")((acc, e) => acc + e)
  }

  /**
   * Lit le contenu d'un fichier sur disque.
   *
   * @param nom le nom d'un fichier
   * @return la chaîne contenue dans le fichier nommé nom
   *
   * @note Le nom de fichier peut être indiqué de manière
   *       relative à la racine du projet courant.
   */
  def lireFichier(nom: String): String = {
    val bufferedSource = Source.fromFile(nom)
    val contenu = bufferedSource.getLines.mkString(sys.props("line.separator"))
    bufferedSource.close()
    contenu
  }

  /**
   * Ecrit une chaîne de caractères dans un fichier.
   * Le fichier est écrasé s'il était déjà existant.
   *
   * @param nom le nom du fichier dans lequel on écrit
   * @param contenu la chaîne de caractères à écrire
   *
   * @note Le nom de fichier peut être indiqué de manière
   *       relative à la racine du projet courant.
   */
  def ecrireFichier(nom: String, contenu: String): Unit = {
    val writer = new PrintWriter(new File(nom))
    writer.write(contenu)
    writer.close()
  }

  /**
   * @param s une chaîne de 0 et 1, encodage binaire 16 bits d'un caractère
   * @return le caractère correspondant à s
   */
  def toChar(s: String): Char = {
    Integer.parseInt(s, 2).toChar
  }

  /**
   * @param c un caractère 0 ou 1
   * @return le bit correspondant à c
   */
  def charToBit(c: Char): Bit = {
    c match {
      case '0' => Zero
      case '1' => One
      case _   => sys.error("Unknown bit character: " + c)
    }
  }

  /**
   * @param s une chaîne de 0 et 1 uniquement
   * @return la liste de bits correspondant à s
   */
  def stringToListBit(s: String): List[Bit] = {
    s.toList.map(charToBit)
  }

  /**
   * @param h Un arbre de Huffman
   * @return la fréquence de la racine de l'arbre
   */
  def extractFreq(h: Huffman): Double = {
    h match {
      case Feuille(f, _)  => f
      case Noeud(f, _, _) => f
    }
  }

  /**
   * Vérifie que h1 et h2 sont égaux, en comparant les fréquences Double
   * avec la précision d. Echoue si ce n'est pas le cas.
   * @param h1 un arbre de Huffman
   * @param h2 un arbre de Huffman
   * @param d un double
   */
  def assertEqualsHuffman(h1: Huffman, h2: Huffman, d: Double): Unit = {
    (h1, h2) match {
      case (Feuille(f1, c1), Feuille(f2, c2)) => {
        assertEquals(f1, f2, d);
        assertEquals(c1, c2)
      }
      case (Noeud(f1, h11, h12), Noeud(f2, h21, h22)) =>
        assertEquals(f1, f2, d);
        assertEqualsHuffman(h11, h21, d);
        assertEqualsHuffman(h12, h22, d)
      case _ => fail("Les deux arbres n'ont pas la même structure")
    }
  }
}