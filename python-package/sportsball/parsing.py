#!/usr/bin/env python
# -*- coding: utf-8 -*-

import pandas as pd
from lxml import html
from selenium import webdriver


class MasseyParser(object):

    NFL = "https://www.masseyratings.com/nfl/ratings"

    def __init__(self, executable_path):
        self.__executable_path = executable_path

    def parse_nfl(self, week: int):
        with webdriver.Chrome(
                executable_path=self.__executable_path) as driver:
            driver.get(self.NFL)
            tree = html.fromstring(driver.page_source)
            datatable = pd.DataFrame({
                'team': [x.text for x in tree.cssselect('.tan > a')],
                'rating':
                [float(x.text) for x in tree.cssselect('.sorted .detail')],
                'off': [
                    float(x.text)
                    for x in tree.cssselect('.frank:nth-child(6) .detail')
                ],
                'def': [
                    float(x.text)
                    for x in tree.cssselect('.frank:nth-child(7) .detail')
                ],
                'hfa': [
                    float(x.text)
                    for x in tree.cssselect('.bodyrow .fShort:nth-child(8)')
                ],
            })
            datatable['week'] = week
            return datatable
