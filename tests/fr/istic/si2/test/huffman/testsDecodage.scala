package fr.istic.si2.test.huffman

import org.junit.Test
import org.junit.Assert._
import fr.istic.si2.huffman.Encodage._
import fr.istic.si2.huffman.Decodage._
import fr.istic.si2.huffman.ConstructionCode._
import fr.istic.si2.testerApp._
import fr.istic.si2.huffman._
import fr.istic.si2.huffman.Utils._

class TestsDecodage {

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
  def decodeSymbolv0Test() {
    assertEquals(None, decodeSymbolv0(figure1, Nil))
    assertEquals(None, decodeSymbolv0(figure1, List(One, One, Zero, One, Zero)))
    assertEquals(Some('a'), decodeSymbolv0(figure1, List(Zero, Zero)))
    assertEquals(Some('f'), decodeSymbolv0(figure1, List(One, One, Zero, Zero)))
  }

  @Test
  def decodeSymbolTest() {
    assertEquals((None, Nil), decodeSymbol(figure1, List(Zero)))
    assertEquals((None, Nil), decodeSymbol(figure1, List(One, One, Zero)))
    assertEquals((None, Nil), decodeSymbol(figure1, List(One, One, Zero)))
    assertEquals((Some('b'), List(One)), decodeSymbol(figure1, List(One, Zero, One)))
    assertEquals((Some('f'), List(Zero, Zero)), decodeSymbol(figure1, List(One, One, Zero, Zero, Zero, Zero)))
  }

  @Test
  def decodeTest() {
    assertEquals(None, decode(List(Zero, One, One, Zero, One), figure1))
    assertEquals(Some("a"), decode(List(Zero, Zero), figure1))
    assertEquals(Some("gaffe"), decode(List(One, One, Zero, One, Zero, Zero, One, One, Zero, Zero, One, One, Zero, Zero, One, One, One), figure1))
  }
  
  @Test
  def lireDescriptionTest() {
    val d1 = "1" + "0" + vers16Bits("a") + "0" + vers16Bits("b")
    val h1 = Noeud(0, Feuille(0, 'a'), Feuille(0, 'b'))
    assertEquals(Some((h1, List())), lireDescription(stringToListBit(d1)))
    assertEquals(None, lireDescription(stringToListBit(d1).dropRight(17)))
  }
}
