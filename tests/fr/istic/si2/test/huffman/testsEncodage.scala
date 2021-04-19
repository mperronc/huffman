package fr.istic.si2.test.huffman

import org.junit.Test
import org.junit.Assert._
import fr.istic.si2.huffman.Encodage._
import fr.istic.si2.huffman.Decodage._
import fr.istic.si2.huffman.ConstructionCode._
import fr.istic.si2.testerApp._
import fr.istic.si2.huffman._
import fr.istic.si2.huffman.Utils._

class TestsEncodage {

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
  def encodeSymbolTest() {
    assertEquals(Some(List(Zero, Zero)), encodeSymbol('a', figure1))
    assertEquals(Some(List(One, One, Zero, Zero)), encodeSymbol('f', figure1))
    assertEquals(None, encodeSymbol('z', figure1))
  }

  @Test
  def encodeListTest() {
    assertEquals(List(Zero, Zero, One, Zero), encode("ab", figure1))
    assertEquals(List(One, One, Zero, Zero,
      One, Zero,
      One, One, One), encode("fbe", figure1))
    assertEquals(Nil, encode("zyx", figure1))
    assertEquals(List(Zero, One, Zero, One, One, Zero, One), encode("ctg", figure1))
  }

  @Test
  def descriptionHuffmanTest() {
    val h = Noeud(1, Feuille(.45, 'a'), Noeud(.55, Feuille(.19, 'r'), Noeud(.36, Noeud(.18, Feuille(.09, 'c'), Feuille(.09, 'd')), Feuille(.18, 'b'))))
    val expect = "10" + vers16Bits("a") + "10" + vers16Bits("r") + "110" + vers16Bits("c") + "0" + vers16Bits("d") + "0" + vers16Bits("b")
    assertEquals(expect, descriptionHuffman(h))
  }
}
