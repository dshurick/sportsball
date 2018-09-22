import gspread
import pandas
from oauth2client.service_account import ServiceAccountCredentials

scope = [
    'https://spreadsheets.google.com/feeds',
    'https://www.googleapis.com/auth/drive'
]

credentials = ServiceAccountCredentials.from_json_keyfile_name(
    '/Users/dshurick/Downloads/dshurick-rdb-test-cb506c785735.json', scope)

spreadsheet = gspread.authorize(credentials)

worksheet = spreadsheet.open("NFL 2018 Expected Wins")

teaminfo = pandas.DataFrame(worksheet.get_worksheet(6).get_all_records())
