from unittest import TestCase

from sportsball.nfl.nfl import Season


class TestSeason(TestCase):
    def test_instantiation(self):
        s = Season(2018)
        self.assertTrue(isinstance(s, Season))
