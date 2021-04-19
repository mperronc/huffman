/**
 * Matthias PERRONCEL
 * Tudwall CREZE
 * Groupe MA1-A
 */

package fr.istic.si2.test.huffman

import org.junit.Test
import org.junit.Assert._
import fr.istic.si2.huffman.Encodage._
import fr.istic.si2.huffman.Decodage._
import fr.istic.si2.huffman.ConstructionCode._
import fr.istic.si2.testerApp._
import fr.istic.si2.huffman._
import fr.istic.si2.huffman.Utils._

class TestsCode {

  val _ = new AppInit(HuffmanApp0) // Ne pas supprimer cette ligne.

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

  @Test
  def initHuffmanTest() {
    val l = List(('a', 0.25), ('b', 0.25), ('c', 0.5))
    val expect = List(Feuille(0.25, 'a'), Feuille(0.25, 'b'), Feuille(0.5, 'c'))
    assertEquals(expect, initHuffman(l))
  }

  @Test
  def triSelonFreqTest() {
    val l = List(
      Feuille(4, 'a'),
      Noeud(2, Feuille(0, 'x'), Feuille(0, 'y')),
      Feuille(3, 'b'),
      Feuille(0, 'c'),
      Noeud(1, Noeud(99, Feuille(0, 'x'), Feuille(0, 'x')), Feuille(0, 'x')))

    val expect = List(
      Feuille(0, 'c'),
      Noeud(1, Noeud(99, Feuille(0, 'x'), Feuille(0, 'x')), Feuille(0, 'x')),
      Noeud(2, Feuille(0, 'x'), Feuille(0, 'y')),
      Feuille(3, 'b'),
      Feuille(4, 'a'))
    assertEquals(expect, triSelonFreq(l))
  }

  @Test
  def uneFusionTest() {
    val h1 = Feuille(15, 'c')
    val h2 = Noeud(5, Feuille(2, 'a'), Feuille(3, 'b'))
    val fusionh1h2 = Noeud(20, h2, h1)

    // Test que une fusion de passe bien
    assertEquals(List(fusionh1h2), uneFusion(List(h1, h2)))

    val h3 = Feuille(99, 'z')
    val h4 = Noeud(999, Feuille(990, 'x'), Feuille(9, 'y'))

    // Test que le reste de la liste reste instact
    assertEquals(List(fusionh1h2, h3, h4), uneFusion(List(h1, h2, h3, h4)))
  }

  @Test
  def fusionTest() {
    val h1 = Feuille(1, 'c')
    val h2 = Feuille(2, 'd')
    val h3 = Noeud(4, h1, h2)
    val expect = Noeud(7, Noeud(3, h1, h2), h3)
    assertEquals(expect, fusion(List(h2, h1, h3)))
  }

  @Test
  def analyseFrequencesTest() {
    val s = "woof woof bork" //wf bork
    // L'ordre des tuples est un peu chelou parce que j'ai compteOccurences en tailrec mais ajouteUn en recursion normale 🙃
    // Enfin je crois que ça vient de là ? 🤔
    val fs = List(('k', 1 / 14.0), ('r', 1 / 14.0), ('b', 1 / 14.0), (' ', 2 / 14.0), ('f', 2 / 14.0), ('o', 5 / 14.0), ('w', 2 / 14.0))
    assertEquals(fs, analyseFrequences(s))
  }

  @Test
  def compteOccurencesTest() {
    val s1 = "abc"
    val occs1 = List(('c', 1), ('b', 1), ('a', 1))
    assertEquals(occs1, compteOccurences(s1.toList))

    val s2 = "tttt"
    val occs2 = List(('t', 4))
    assertEquals(occs2, compteOccurences(s2.toList))

    val s3 = "abba"
    val occs3 = List(('b', 2), ('a', 2))
    assertEquals(occs3, compteOccurences(s3.toList))
  }

  @Test
  def apparaitTest() {
    val s = List(('a', 1), ('b', 1), ('c', 1))
    assertEquals(true, apparait('a', s))
    assertEquals(true, apparait('b', s))
    assertEquals(true, apparait('c', s))
    assertEquals(false, apparait('f', s))
  }

  @Test
  def ajouteUnTest() {
    val c = List(('a', 3), ('b', 4), ('c', 5))
    val ajouteB = List(('a', 3), ('b', 5), ('c', 5))
    assertEquals(ajouteB, ajouteUn('b', c))
    assertEquals(c, ajouteUn('d', c))
  }
}
