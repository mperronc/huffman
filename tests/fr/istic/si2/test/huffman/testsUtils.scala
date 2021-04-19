package fr.istic.si2.test.huffman

import org.junit.Test
import org.junit.Assert._
import fr.istic.si2.huffman.Encodage._
import fr.istic.si2.huffman.Decodage._
import fr.istic.si2.huffman.ConstructionCode._
import fr.istic.si2.testerApp._
import fr.istic.si2.huffman._
import fr.istic.si2.huffman.Utils._

class TestsUtils {

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
  def listBitToStringTest() {
    assertEquals("", listBitToString(Nil))
    assertEquals("1101", listBitToString(List(One, One, Zero, One)))
  }

  @Test
  def takeTest() {
    assertEquals(Nil, take(0, List(1, 2, 3)))
    assertEquals(List(1), take(1, List(1, 2, 3)))
    assertEquals(List(1, 2), take(2, List(1, 2, 3)))
    assertEquals(List(1, 2, 3), take(3, List(1, 2, 3)))
    assertEquals(List(1, 2, 3), take(4, List(1, 2, 3)))
  }

  @Test
  def dropTest() {
    assertEquals(Nil, drop(3, List(1, 2, 3)))
    assertEquals(List(3), drop(2, List(1, 2, 3)))
    assertEquals(List(2, 3), drop(1, List(1, 2, 3)))
    assertEquals(List(1, 2, 3), drop(0, List(1, 2, 3)))
  }
}
