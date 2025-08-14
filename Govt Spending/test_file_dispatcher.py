import unittest
import os
import pandas as pd
from file_dispatcher import _identify_header

class TestFileDispatcher(unittest.TestCase):
    def setUp(self):
        self.test_csv_path = "test_header.csv"

    def tearDown(self):
        if os.path.exists(self.test_csv_path):
            os.remove(self.test_csv_path)

    def test_identify_header_multi_line(self):
        csv_content = """"Main Category",,"Details",
"Programme","Sub-Programme","Amount","Status"
"Health","Curative",1000,"Ongoing"
"""
        with open(self.test_csv_path, "w") as f:
            f.write(csv_content)

        expected_header = [
            "Main Category Programme",
            "Main Category Sub-Programme",
            "Details Amount",
            "Details Status"
        ]

        cleaned_columns, header_rows_count = _identify_header(self.test_csv_path)

        self.assertEqual(header_rows_count, 2)
        self.assertEqual(cleaned_columns, expected_header)

if __name__ == '__main__':
    # Change working directory to the script's directory
    # to ensure the test creates files in the correct place.
    os.chdir(os.path.dirname(os.path.abspath(__file__)))
    unittest.main()
