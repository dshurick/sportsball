#!/usr/bin/env python
# -*- coding: utf-8 -*-
import numpy
import pandas

import pandas as pd
from gspread_pandas import Spread
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
            datatable = datatable.astype(
                dtype={
                    'week': int,
                    'rating': numpy.float,
                    'off': numpy.float,
                    'def': numpy.float,
                    'hfa': numpy.float,
                })
            return datatable

    def update_spreadsheet(self, week):
        spread = Spread('dshurick', 'NFL 2018 Expected Wins')
        spread.open('NFL 2018 Expected Wins')

        df = spread.sheet_to_df(
            sheet='massey', header_rows=1, index=None).astype(
                dtype={
                    'week': int,
                    'rating': numpy.float,
                    'off': numpy.float,
                    'def': numpy.float,
                    'hfa': numpy.float,
                })

        df = df.loc[df.week < week]

        tbl = self.parse_nfl(week=week).astype(
            dtype={
                'week': int,
                'rating': numpy.float,
                'off': numpy.float,
                'def': numpy.float,
                'hfa': numpy.float,
            })

        df = pandas.concat([df, tbl], ignore_index=True, sort=False)

        spread.df_to_sheet(df=df, index=False, replace=True, sheet='massey')
