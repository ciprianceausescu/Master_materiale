import unittest
import parenthesization

class MatrixChainMultiplicationTests(unittest.TestCase):
  def test_one_element_chain(self):
    multiplier = parenthesization.MatrixChainMultiplier([1, 2])
    multiplier.calculate_costs()
    self.assertEqual(multiplier.parenthesize(), '0')

  def test_two_element_chain(self):
    multiplier = parenthesization.MatrixChainMultiplier([1, 2, 3])
    multiplier.calculate_costs()
    self.assertEqual(multiplier.parenthesize(), '(0, 1)')

  def test_wikipedia_sample_chain(self):
    multiplier = parenthesization.MatrixChainMultiplier([10, 30, 5, 60])
    multiplier.calculate_costs()
    self.assertEqual(multiplier.parenthesize(), '((0, 1), 2)')

  def test_book_sample_chain(self):
    multiplier = parenthesization.MatrixChainMultiplier([30, 35, 15, 5, 10, 20, 25])
    multiplier.calculate_costs()
    self.assertEqual(multiplier.parenthesize(), '((0, (1, 2)), ((3, 4), 5))')

  def test_one_element_chain_calculates_costs(self):
    multiplier = parenthesization.MatrixChainMultiplier([1, 2])
    multiplier.calculate_costs()
    self.assertEqual(multiplier.costs[(0,0)], 0)

  def test_two_element_chain_calculates_costs(self):
    multiplier = parenthesization.MatrixChainMultiplier([1, 2, 3])
    multiplier.calculate_costs()
    self.assertEqual(multiplier.costs[(0,1)], 6)

  def test_three_element_chain_calculates_costs(self):
    multiplier = parenthesization.MatrixChainMultiplier([1, 2, 3, 4])
    multiplier.calculate_costs()
    self.assertEqual(multiplier.costs[(0,1)], 6)
    self.assertEqual(multiplier.costs[(1,2)], 24)
    self.assertEqual(multiplier.costs[(0,2)], 18)

  def test_wikipedia_sample_chain_cost(self):
    multiplier = parenthesization.MatrixChainMultiplier([10, 30, 5, 60])
    multiplier.calculate_costs()
    self.assertEqual(multiplier.costs[(0,2)], 4500)

  def test_two_element_chain_calculates_pivot(self):
    multiplier = parenthesization.MatrixChainMultiplier([1, 2, 3])
    multiplier.calculate_costs()
    self.assertEqual(multiplier.pivots[(0,1)], 0)

  def test_three_element_chain_calculates_pivot(self):
    multiplier = parenthesization.MatrixChainMultiplier([1, 2, 3, 4])
    multiplier.calculate_costs()
    self.assertEqual(multiplier.pivots[(0,2)], 1)

  def test_four_element_chain_calculates_pivot(self):
    multiplier = parenthesization.MatrixChainMultiplier([1, 2, 3, 4, 5])
    multiplier.calculate_costs()
    self.assertEqual(multiplier.pivots[(0,3)], 2)
    self.assertEqual(multiplier.pivots[(0,2)], 1)

if __name__ == '__main__':
  unittest.main()
