import pandas as pd
from lxml import html
from selenium import webdriver


class MasseyParser(object):

    NFL = "https://www.masseyratings.com/nfl/ratings"

    def __init__(self, executable_path):
        self.executable_path = executable_path

    def parse_nfl(self):
        driver = webdriver.Chrome(executable_path=self.executable_path)
        driver.get(self.NFL)

        tree = html.fromstring(driver.page_source)

        datatable = pd.DataFrame(
            dict(
                team_names=[x.text for x in tree.cssselect('.tan > a')],
                ratings=[
                    float(x.text) for x in tree.cssselect('.sorted .detail')
                ],
                offense=[
                    float(x.text)
                    for x in tree.cssselect('.frank:nth-child(6) .detail')
                ],
                defense=[
                    float(x.text)
                    for x in tree.cssselect('.frank:nth-child(7) .detail')
                ],
                hfa=[
                    float(x.text)
                    for x in tree.cssselect('.bodyrow .fShort:nth-child(8)')
                ],
            ))
        return datatable
