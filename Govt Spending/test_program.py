"""
Test framework for program.py CSV extraction validation.

Usage:
    python test_program.py
    pytest test_program.py -v
"""

import os
import pytest
import subprocess
from pathlib import Path

# Expected line counts for CSV files (to be populated manually)
EXPECTED_LINE_COUNTS = {
    # Format: 'year_quarter_county': expected_lines
    # Example entries - replace with actual verified counts
    '2019_20_01_baringo': 166,
    # '2019_20_01_bomet': 89,
    # '2019_20_01_busia': 156,

    '2019_20_02_baringo': 174,

    '2019_20_03_Baringo': 165,

    '2019_20_04_baringo': 155,

    '2020_21_01_baringo': 151,

    '2020_21_02_baringo': 180,

    '2020_21_03_baringo': 248,

    '2020_21_04_baringo': 255,

    '2021_22_01_baringo': 291,

    '2021_22_02_baringo': 228,

    '2021_22_03_baringo': 192,

    '2021_22_04_baringo': 222,

    '2022_23_01_baringo': 238,

    '2022_23_02_baringo': 206,

    '2022_23_03_baringo': 136,

    '2022_23_04_baringo': 169,

    '2023_24_01_baringo': 363,

    '2023_24_02_baringo': 383,

    '2023_24_03_baringo': 464,

    '2023_24_04_baringo': 469,

    '2024_25_01_baringo': 408,

    '2024_25_02_baringo': 487,

    '2024_25_03_baringo': 336,

    '2019_20_01_nakuru': 159,

    '2019_20_02_nakuru': 208,

    '2019_20_03_nakuru': 169,

    '2019_20_04_nakuru': 185,

    '2020_21_01_nakuru': 167,

    '2020_21_02_nakuru': 180,

    '2020_21_03_nakuru': 201,

    '2020_21_04_nakuru': 234,

    '2021_22_01_nakuru': 315,

    '2021_22_02_nakuru': 602,

    '2021_22_03_nakuru': 425,

    '2021_22_04_nakuru': 442,

    '2022_23_01_nakuru': 438,

    '2022_23_02_nakuru': 442,

    '2022_23_03_nakuru': 337,

    '2022_23_04_nakuru': 575,

    '2023_24_01_nakuru': 459,

    '2023_24_02_nakuru': 496,

    '2023_24_03_nakuru': 367,

    '2023_24_04_nakuru': 402,

    '2024_25_01_nakuru': 500,

    '2024_25_02_nakuru': 560,

    '2024_25_03_nakuru': 392,

    '2019_20_01_bungoma': 98,

    '2019_20_01_elgeyo_marakwet': 129,

    '2019_20_01_embu': 37,

    '2019_20_01_garissa': 99,

    '2019_20_01_homa_bay': 280,

    '2019_20_01_isiolo': 159,

    '2019_20_01_kajiado': 123,

    '2019_20_01_kakamega': 0,

    '2019_20_01_kericho': 108,

    '2019_20_01_kiambu': 148,

    '2019_20_01_kilifi': 98,

    '2019_20_01_kirinyaga': 140,

    '2019_20_01_kisii': 108,

    '2019_20_01_kisumu': 0,

    '2019_20_01_kitui': 165,

    '2019_20_01_kwale': 134,

    '2019_20_01_laikipia': 133,

    '2019_20_01_lamu': 97,

    '2019_20_01_machakos': 153,

    '2019_20_01_makueni': 107,

    '2019_20_01_mandera': 0,

    '2019_20_01_marsabit': 96,

    '2019_20_01_meru': 150,

    '2019_20_01_migori': 420,

    '2019_20_01_mombasa': 98,

    '2019_20_01_muranga': 0,

    '2019_20_01_nairobi_city': 252,

    '2019_20_01_nandi': 132,

    '2019_20_01_narok': 64,

    '2019_20_01_nyamira': 229,

    '2019_20_01_nyandarua': 133,

    '2019_20_01_nyeri': 57,

    '2019_20_01_samburu': 217,

    '2019_20_01_siaya': 185,

    '2019_20_01_taita_taveta': 0,

    '2019_20_01_tana_river': 25,

    '2019_20_01_tharaka_nithi': 0,

    '2019_20_01_trans_nzoia': 82,

    '2019_20_01_turkana': 708,

    '2019_20_01_uasin_gishu': 86,

    '2019_20_01_vihiga': 158,

    '2019_20_01_wajir': 200,

    '2019_20_01_west_pokot': 93,

    '2019_20_02_bomet': 102,

    '2019_20_02_bungoma': 187,

    '2019_20_02_busia': 112,

    '2019_20_02_elgeyo_marakwet': 133,

    '2019_20_02_embu': 144,

    '2019_20_02_garissa': 0,

    '2019_20_02_homa_bay': 285,

    '2019_20_02_isiolo': 136,

    '2019_20_02_kajiado': 194,

    '2019_20_02_kakamega': 0,

    '2019_20_02_kericho': 84,

    '2019_20_02_kiambu': 135,

    '2019_20_02_kilifi': 93,

    '2019_20_02_kirinyaga': 162,

    '2019_20_02_kisii': 101,

    '2019_20_02_kisumu': 214,

    '2019_20_02_kitui': 124,

    '2019_20_02_kwale': 0,

    '2019_20_02_laikipia': 177,

    '2019_20_02_lamu': 68,

    '2019_20_02_machakos': 144,

    '2019_20_02_makueni': 143,

    '2019_20_02_mandera': 142,

    '2019_20_02_marsabit': 87,

    '2019_20_02_meru': 146,

    '2019_20_02_migori': 377,

    '2019_20_02_mombasa': 88,

    '2019_20_02_muranga': 0,

    '2019_20_02_nairobi_city': 339,

    '2019_20_02_nandi': 158,

    '2019_20_02_narok': 112,

    '2019_20_02_nyamira': 252,

    '2019_20_02_nyandarua': 132,

    '2019_20_02_nyeri': 159,

    '2019_20_02_samburu': 319,

    '2019_20_02_siaya': 206,

    '2019_20_02_taita_taveta': 81,

    '2019_20_02_tana_river': 321,

    '2019_20_02_tharaka_nithi': 0,

    '2019_20_02_trans_nzoia': 157,

    '2019_20_02_turkana': 695,

    '2019_20_02_uasin_gishu': 293,

    '2019_20_02_vihiga': 138,

    '2019_20_02_wajir': 145,

    '2019_20_02_west_pokot': 124,

    '2019_20_03_baringo': 165,

    '2019_20_03_bomet': 357,

    '2019_20_03_bungoma': 223,

    '2019_20_03_busia': 226,

    '2019_20_03_elgeyo_marakwet': 167,

    '2019_20_03_embu': 164,

    '2019_20_03_garissa': 174,

    '2019_20_03_homa_bay': 292,

    '2019_20_03_isiolo': 164,

    '2019_20_03_kajiado': 201,

    '2019_20_03_kakamega': 133,

    '2019_20_03_kericho': 139,

    '2019_20_03_kiambu': 171,

    '2019_20_03_kilifi': 148,

    '2019_20_03_kirinyaga': 126,

    '2019_20_03_kisii': 92,

    '2019_20_03_kisumu': 60,

    '2019_20_03_kitui': 143,

    '2019_20_03_kwale': 0,

    '2019_20_03_laikipia': 153,

    '2019_20_03_lamu': 138,

    '2019_20_03_machakos': 135,

    '2019_20_03_makueni': 367,

    '2019_20_03_mandera': 48,

    '2019_20_03_marsabit': 93,

    '2019_20_03_meru': 134,

    '2019_20_03_migori': 362,

    '2019_20_03_mombasa': 102,

    '2019_20_03_muranga': 0,

    '2019_20_03_nairobi_city': 350,

    '2019_20_03_nandi': 145,

    '2019_20_03_narok': 124,

    '2019_20_03_nyamira': 121,

    '2019_20_03_nyandarua': 127,

    '2019_20_03_nyeri': 175,

    '2019_20_03_samburu': 226,

    '2019_20_03_siaya': 185,

    '2019_20_03_taita_taveta': 64,

    '2019_20_03_tana_river': 329,

    '2019_20_03_tharaka_nithi': 0,

    '2019_20_03_trans_nzoia': 105,

    '2019_20_03_turkana': 450,

    '2019_20_03_uasin_gishu': 278,

    '2019_20_03_vihiga': 178,

    '2019_20_03_wajir': 194,

    '2019_20_03_west_pokot': 136,

    '2019_20_04_bomet': 488,

    '2019_20_04_bungoma': 218,

    '2019_20_04_busia': 229,

    '2019_20_04_elgeyo_marakwet': 100,

    '2019_20_04_embu': 180,

    '2019_20_04_garissa': 190,

    '2019_20_04_homa_bay': 330,

    '2019_20_04_isiolo': 341,

    '2019_20_04_kajiado': 285,

    '2019_20_04_kakamega': 260,

    '2019_20_04_kericho': 147,

    '2019_20_04_kiambu': 83,

    '2019_20_04_kilifi': 142,

    '2019_20_04_kirinyaga': 179,

    '2019_20_04_kisii': 115,

    '2019_20_04_kisumu': 261,

    '2019_20_04_kitui': 0,

    '2019_20_04_kwale': 250,

    '2019_20_04_laikipia': 201,

    '2019_20_04_lamu': 91,

    '2019_20_04_machakos': 158,

    '2019_20_04_makueni': 220,

    '2019_20_04_mandera': 67,

    '2019_20_04_marsabit': 111,

    '2019_20_04_meru': 185,

    '2019_20_04_migori': 521,

    '2019_20_04_mombasa': 115,

    '2019_20_04_muranga': 0,

    '2019_20_04_nairobi_city': 0,

    '2019_20_04_nandi': 157,

    '2019_20_04_narok': 245,

    '2019_20_04_nyamira': 200,

    '2019_20_04_nyandarua': 154,

    '2019_20_04_nyeri': 174,

    '2019_20_04_samburu': 302,

    '2019_20_04_siaya': 187,

    '2019_20_04_taita_taveta': 62,

    '2019_20_04_tana_river': 248,

    '2019_20_04_tharaka_nithi': 0,

    '2019_20_04_trans_nzoia': 17,

    '2019_20_04_turkana': 0,

    '2019_20_04_uasin_gishu': 234,

    '2019_20_04_vihiga': 175,

    '2019_20_04_wajir': 242,

    '2019_20_04_west_pokot': 133,

    '2020_21_01_bomet': 0,

    '2020_21_01_bungoma': 375,

    '2020_21_01_busia': 147,

    '2020_21_01_elgeyo_marakwet': 167,

    '2020_21_01_embu': 145,

    '2020_21_01_garissa': 0,

    '2020_21_01_homa_bay': 300,

    '2020_21_01_isiolo': 121,

    '2020_21_01_kajiado': 184,

    '2020_21_01_kakamega': 129,

    '2020_21_01_kericho': 129,

    '2020_21_01_kiambu': 160,

    '2020_21_01_kilifi': 111,

    '2020_21_01_kirinyaga': 151,

    '2020_21_01_kisii': 108,

    '2020_21_01_kisumu': 128,

    '2020_21_01_kitui': 0,

    '2020_21_01_kwale': 0,

    '2020_21_01_laikipia': 151,

    '2020_21_01_lamu': 103,

    '2020_21_01_machakos': 0,

    '2020_21_01_makueni': 0,

    '2020_21_01_mandera': 0,

    '2020_21_01_marsabit': 91,

    '2020_21_01_meru': 187,

    '2020_21_01_migori': 416,

    '2020_21_01_mombasa': 89,

    '2020_21_01_muranga': 0,

    '2020_21_01_nairobi_city': 0,

    '2020_21_01_nandi': 145,

    '2020_21_01_narok': 137,

    '2020_21_01_nyamira': 173,

    '2020_21_01_nyandarua': 143,

    '2020_21_01_nyeri': 150,

    '2020_21_01_samburu': 238,

    '2020_21_01_siaya': 0,

    '2020_21_01_taita_taveta': 57,

    '2020_21_01_tana_river': 100,

    '2020_21_01_tharaka_nithi': 0,

    '2020_21_01_trans_nzoia': 89,

    '2020_21_01_turkana': 479,

    '2020_21_01_uasin_gishu': 212,

    '2020_21_01_vihiga': 156,

    '2020_21_01_wajir': 0,

    '2020_21_01_west_pokot': 126,

    '2020_21_02_bomet': 10,

    '2020_21_02_bungoma': 324,

    '2020_21_02_busia': 216,

    '2020_21_02_elgeyo_marakwet': 157,

    '2020_21_02_embu': 150,

    '2020_21_02_garissa': 0,

    '2020_21_02_homa_bay': 336,

    '2020_21_02_isiolo': 0,

    '2020_21_02_kajiado': 219,

    '2020_21_02_kakamega': 196,

    '2020_21_02_kericho': 113,

    '2020_21_02_kiambu': 187,

    '2020_21_02_kilifi': 131,

    '2020_21_02_kirinyaga': 174,

    '2020_21_02_kisii': 99,

    '2020_21_02_kisumu': 240,

    '2020_21_02_kitui': 141,

    '2020_21_02_kwale': 143,

    '2020_21_02_laikipia': 136,

    '2020_21_02_lamu': 139,

    '2020_21_02_machakos': 136,

    '2020_21_02_makueni': 190,

    '2020_21_02_mandera': 60,

    '2020_21_02_marsabit': 88,

    '2020_21_02_meru': 178,

    '2020_21_02_migori': 520,

    '2020_21_02_mombasa': 94,

    '2020_21_02_muranga': 0,

    '2020_21_02_nairobi_city': 180,

    '2020_21_02_nandi': 129,

    '2020_21_02_narok': 121,

    '2020_21_02_nyamira': 183,

    '2020_21_02_nyandarua': 136,

    '2020_21_02_nyeri': 164,

    '2020_21_02_samburu': 265,

    '2020_21_02_siaya': 149,

    '2020_21_02_taita_taveta': 66,

    '2020_21_02_tana_river': 182,

    '2020_21_02_tharaka_nithi': 0,

    '2020_21_02_trans_nzoia': 85,

    '2020_21_02_turkana': 482,

    '2020_21_02_uasin_gishu': 218,

    '2020_21_02_vihiga': 0,

    '2020_21_02_wajir': 208,

    '2020_21_02_west_pokot': 130,

    '2020_21_03_bomet': 0,

    '2020_21_03_bungoma': 297,

    '2020_21_03_busia': 222,

    '2020_21_03_elgeyo_marakwet': 154,

    '2020_21_03_embu': 169,

    '2020_21_03_garissa': 127,

    '2020_21_03_homa_bay': 272,

    '2020_21_03_isiolo': 266,

    '2020_21_03_kajiado': 83,

    '2020_21_03_kakamega': 95,

    '2020_21_03_kericho': 128,

    '2020_21_03_kiambu': 163,

    '2020_21_03_kilifi': 117,

    '2020_21_03_kirinyaga': 138,

    '2020_21_03_kisii': 99,

    '2020_21_03_kisumu': 320,

    '2020_21_03_kitui': 141,

    '2020_21_03_kwale': 0,

    '2020_21_03_laikipia': 201,

    '2020_21_03_lamu': 145,

    '2020_21_03_machakos': 203,

    '2020_21_03_makueni': 193,

    '2020_21_03_mandera': 60,

    '2020_21_03_marsabit': 83,

    '2020_21_03_meru': 190,

    '2020_21_03_migori': 514,

    '2020_21_03_mombasa': 97,

    '2020_21_03_muranga': 0,

    '2020_21_03_nairobi_city': 0,

    '2020_21_03_nandi': 129,

    '2020_21_03_narok': 77,

    '2020_21_03_nyamira': 123,

    '2020_21_03_nyandarua': 154,

    '2020_21_03_nyeri': 172,

    '2020_21_03_samburu': 253,

    '2020_21_03_siaya': 175,

    '2020_21_03_taita_taveta': 0,

    '2020_21_03_tana_river': 198,

    '2020_21_03_tharaka_nithi': 131,

    '2020_21_03_trans_nzoia': 104,

    '2020_21_03_turkana': 485,

    '2020_21_03_uasin_gishu': 249,

    '2020_21_03_vihiga': 153,

    '2020_21_03_wajir': 155,

    '2020_21_03_west_pokot': 128,

    '2020_21_04_bomet': 0,

    '2020_21_04_bungoma': 333,

    '2020_21_04_busia': 299,

    '2020_21_04_elgeyo_marakwet': 153,

    '2020_21_04_embu': 206,

    '2020_21_04_garissa': 154,

    '2020_21_04_homa_bay': 250,

    '2020_21_04_isiolo': 324,

    '2020_21_04_kajiado': 104,

    '2020_21_04_kakamega': 234,

    '2020_21_04_kericho': 155,

    '2020_21_04_kiambu': 191,

    '2020_21_04_kilifi': 140,

    '2020_21_04_kirinyaga': 155,

    '2020_21_04_kisii': 0,

    '2020_21_04_kisumu': 192,

    '2020_21_04_kitui': 153,

    '2020_21_04_kwale': 0,

    '2020_21_04_laikipia': 196,

    '2020_21_04_lamu': 79,

    '2020_21_04_machakos': 148,

    '2020_21_04_makueni': 246,

    '2020_21_04_mandera': 81,

    '2020_21_04_marsabit': 94,

    '2020_21_04_meru': 221,

    '2020_21_04_migori': 265,

    '2020_21_04_mombasa': 103,

    '2020_21_04_muranga': 0,

    '2020_21_04_nairobi_city': 0,

    '2020_21_04_nandi': 131,

    '2020_21_04_narok': 131,

    '2020_21_04_nyamira': 123,

    '2020_21_04_nyandarua': 163,

    '2020_21_04_nyeri': 219,

    '2020_21_04_samburu': 0,

    '2020_21_04_siaya': 186,

    '2020_21_04_taita_taveta': 71,

    '2020_21_04_tana_river': 0,

    '2020_21_04_tharaka_nithi': 0,

    '2020_21_04_trans_nzoia': 85,

    '2020_21_04_turkana': 525,

    '2020_21_04_uasin_gishu': 239,

    '2020_21_04_vihiga': 150,

    '2020_21_04_wajir': 175,

    '2020_21_04_west_pokot': 137,

    '2021_22_01_bomet': 307,

    '2021_22_01_bungoma': 534,

    '2021_22_01_busia': 273,

    '2021_22_01_elgeyo_marakwet': 136,

    '2021_22_01_embu': 216,

    '2021_22_01_garissa': 134,

    '2021_22_01_homa_bay': 306,

    '2021_22_01_isiolo': 380,

    '2021_22_01_kajiado': 149,

    '2021_22_01_kakamega': 0,

    '2021_22_01_kericho': 184,

    '2021_22_01_kiambu': 176,

    '2021_22_01_kilifi': 128,

    '2021_22_01_kirinyaga': 193,

    '2021_22_01_kisii': 112,

    '2021_22_01_kisumu': 236,

    '2021_22_01_kitui': 182,

    '2021_22_01_kwale': 0,

    '2021_22_01_laikipia': 233,

    '2021_22_01_lamu': 106,

    '2021_22_01_machakos': 240,

    '2021_22_01_makueni': 168,

    '2021_22_01_mandera': 67,

    '2021_22_01_marsabit': 89,

    '2021_22_01_meru': 151,

    '2021_22_01_migori': 353,

    '2021_22_01_mombasa': 97,

    '2021_22_01_muranga': 0,

    '2021_22_01_nairobi_city': 262,

    '2021_22_01_nandi': 81,

    '2021_22_01_narok': 227,

    '2021_22_01_nyamira': 0,

    '2021_22_01_nyandarua': 151,

    '2021_22_01_nyeri': 176,

    '2021_22_01_samburu': 316,

    '2021_22_01_siaya': 112,

    '2021_22_01_taita_taveta': 60,

    '2021_22_01_tana_river': 206,

    '2021_22_01_tharaka_nithi': 0,

    '2021_22_01_trans_nzoia': 117,

    '2021_22_01_turkana': 705,

    '2021_22_01_uasin_gishu': 196,

    '2021_22_01_vihiga': 0,

    '2021_22_01_wajir': 125,

    '2021_22_01_west_pokot': 0,

    '2021_22_02_bomet': 339,

    '2021_22_02_bungoma': 644,

    '2021_22_02_busia': 0,

    '2021_22_02_elgeyo_marakwet': 179,

    '2021_22_02_embu': 258,

    '2021_22_02_garissa': 130,

    '2021_22_02_homa_bay': 334,

    '2021_22_02_isiolo': 0,

    '2021_22_02_kajiado': 0,

    '2021_22_02_kakamega': 0,

    '2021_22_02_kericho': 148,

    '2021_22_02_kiambu': 161,

    '2021_22_02_kilifi': 124,

    '2021_22_02_kirinyaga': 147,

    '2021_22_02_kisii': 99,

    '2021_22_02_kisumu': 149,

    '2021_22_02_kitui': 0,

    '2021_22_02_kwale': 0,

    '2021_22_02_laikipia': 247,

    '2021_22_02_lamu': 114,

    '2021_22_02_machakos': 169,

    '2021_22_02_makueni': 121,

    '2021_22_02_mandera': 59,

    '2021_22_02_marsabit': 156,

    '2021_22_02_meru': 158,

    '2021_22_02_migori': 217,

    '2021_22_02_mombasa': 135,

    '2021_22_02_muranga': 0,

    '2021_22_02_nairobi_city': 289,

    '2021_22_02_nandi': 115,

    '2021_22_02_narok': 210,

    '2021_22_02_nyamira': 0,

    '2021_22_02_nyandarua': 154,

    '2021_22_02_nyeri': 186,

    '2021_22_02_samburu': 306,

    '2021_22_02_siaya': 110,

    '2021_22_02_taita_taveta': 56,

    '2021_22_02_tana_river': 153,

    '2021_22_02_tharaka_nithi': 0,

    '2021_22_02_trans_nzoia': 0,

    '2021_22_02_turkana': 0,

    '2021_22_02_uasin_gishu': 177,

    '2021_22_02_vihiga': 155,

    '2021_22_02_wajir': 142,

    '2021_22_02_west_pokot': 0,

    '2021_22_03_bomet': 0,

    '2021_22_03_bungoma': 496,

    '2021_22_03_busia': 304,

    '2021_22_03_elgeyo_marakwet': 173,

    '2021_22_03_embu': 225,

    '2021_22_03_garissa': 129,

    '2021_22_03_homa_bay': 295,

    '2021_22_03_isiolo': 150,

    '2021_22_03_kajiado': 0,

    '2021_22_03_kakamega': 0,

    '2021_22_03_kericho': 151,

    '2021_22_03_kiambu': 166,

    '2021_22_03_kilifi': 138,

    '2021_22_03_kirinyaga': 172,

    '2021_22_03_kisii': 117,

    '2021_22_03_kisumu': 210,

    '2021_22_03_kitui': 0,

    '2021_22_03_kwale': 0,

    '2021_22_03_laikipia': 235,

    '2021_22_03_lamu': 114,

    '2021_22_03_machakos': 256,

    '2021_22_03_makueni': 183,

    '2021_22_03_mandera': 56,

    '2021_22_03_marsabit': 155,

    '2021_22_03_meru': 148,

    '2021_22_03_migori': 237,

    '2021_22_03_mombasa': 117,

    '2021_22_03_muranga': 0,

    '2021_22_03_nairobi_city': 245,

    '2021_22_03_nandi': 100,

    '2021_22_03_narok': 152,

    '2021_22_03_nyamira': 0,

    '2021_22_03_nyandarua': 152,

    '2021_22_03_nyeri': 193,

    '2021_22_03_samburu': 309,

    '2021_22_03_siaya': 164,

    '2021_22_03_taita_taveta': 67,

    '2021_22_03_tana_river': 0,

    '2021_22_03_tharaka_nithi': 227,

    '2021_22_03_trans_nzoia': 0,

    '2021_22_03_turkana': 0,

    '2021_22_03_uasin_gishu': 195,

    '2021_22_03_vihiga': 0,

    '2021_22_03_wajir': 0,

    '2021_22_03_west_pokot': 207,

    '2021_22_04_bomet': 0,

    '2021_22_04_bungoma': 443,

    '2021_22_04_busia': 291,

    '2021_22_04_elgeyo_marakwet': 144,

    '2021_22_04_embu': 227,

    '2021_22_04_garissa': 168,

    '2021_22_04_homa_bay': 328,

    '2021_22_04_isiolo': 198,

    '2021_22_04_kajiado': 132,

    '2021_22_04_kakamega': 106,

    '2021_22_04_kericho': 144,

    '2021_22_04_kiambu': 167,

    '2021_22_04_kilifi': 129,

    '2021_22_04_kirinyaga': 173,

    '2021_22_04_kisii': 116,

    '2021_22_04_kisumu': 241,

    '2021_22_04_kitui': 0,

    '2021_22_04_kwale': 0,

    '2021_22_04_laikipia': 243,

    '2021_22_04_lamu': 222,

    '2021_22_04_machakos': 115,

    '2021_22_04_makueni': 0,

    '2021_22_04_mandera': 58,

    '2021_22_04_marsabit': 191,

    '2021_22_04_meru': 165,

    '2021_22_04_migori': 177,

    '2021_22_04_mombasa': 105,

    '2021_22_04_muranga': 0,

    '2021_22_04_nairobi_city': 271,

    '2021_22_04_nandi': 98,

    '2021_22_04_narok': 196,

    '2021_22_04_nyamira': 0,

    '2021_22_04_nyandarua': 139,

    '2021_22_04_nyeri': 172,

    '2021_22_04_samburu': 371,

    '2021_22_04_siaya': 141,

    '2021_22_04_taita_taveta': 57,

    '2021_22_04_tana_river': 213,

    '2021_22_04_tharaka_nithi': 98,

    '2021_22_04_trans_nzoia': 124,

    '2021_22_04_turkana': 0,

    '2021_22_04_uasin_gishu': 197,

    '2021_22_04_vihiga': 0,

    '2021_22_04_wajir': 261,

    '2021_22_04_west_pokot': 226,

    '2022_23_01_bomet': 167,

    '2022_23_01_bungoma': 427,

    '2022_23_01_busia': 278,

    '2022_23_01_elgeyo_marakwet': 135,

    '2022_23_01_embu': 194,

    '2022_23_01_garissa': 173,

    '2022_23_01_homa_bay': 0,

    '2022_23_01_isiolo': 128,

    '2022_23_01_kajiado': 0,

    '2022_23_01_kakamega': 202,

    '2022_23_01_kericho': 145,

    '2022_23_01_kiambu': 195,

    '2022_23_01_kilifi': 128,

    '2022_23_01_kirinyaga': 0,

    '2022_23_01_kisii': 190,

    '2022_23_01_kisumu': 0,

    '2022_23_01_kitui': 0,

    '2022_23_01_kwale': 0,

    '2022_23_01_laikipia': 192,

    '2022_23_01_lamu': 154,

    '2022_23_01_machakos': 148,

    '2022_23_01_makueni': 0,

    '2022_23_01_mandera': 67,

    '2022_23_01_marsabit': 0,

    '2022_23_01_meru': 131,

    '2022_23_01_migori': 271,

    '2022_23_01_mombasa': 105,

    '2022_23_01_muranga': 0,

    '2022_23_01_nairobi_city': 0,

    '2022_23_01_nandi': 208,

    '2022_23_01_narok': 286,

    '2022_23_01_nyamira': 0,

    '2022_23_01_nyandarua': 133,

    '2022_23_01_nyeri': 0,

    '2022_23_01_samburu': 136,

    '2022_23_01_siaya': 180,

    '2022_23_01_taita_taveta': 353,

    '2022_23_01_tana_river': 0,

    '2022_23_01_tharaka_nithi': 207,

    '2022_23_01_trans_nzoia': 193,

    '2022_23_01_turkana': 950,

    '2022_23_01_uasin_gishu': 140,

    '2022_23_01_vihiga': 163,

    '2022_23_01_wajir': 0,

    '2022_23_01_west_pokot': 0,

    '2022_23_02_bomet': 167,

    '2022_23_02_bungoma': 415,

    '2022_23_02_busia': 260,

    '2022_23_02_elgeyo_marakwet': 0,

    '2022_23_02_embu': 214,

    '2022_23_02_garissa': 185,

    '2022_23_02_homa_bay': 290,

    '2022_23_02_isiolo': 0,

    '2022_23_02_kajiado': 0,

    '2022_23_02_kakamega': 268,

    '2022_23_02_kericho': 168,

    '2022_23_02_kiambu': 178,

    '2022_23_02_kilifi': 132,

    '2022_23_02_kirinyaga': 0,

    '2022_23_02_kisii': 222,

    '2022_23_02_kisumu': 0,

    '2022_23_02_kitui': 0,

    '2022_23_02_kwale': 0,

    '2022_23_02_laikipia': 231,

    '2022_23_02_lamu': 250,

    '2022_23_02_machakos': 187,

    '2022_23_02_makueni': 0,

    '2022_23_02_mandera': 44,

    '2022_23_02_marsabit': 182,

    '2022_23_02_meru': 157,

    '2022_23_02_migori': 268,

    '2022_23_02_mombasa': 113,

    '2022_23_02_muranga': 0,

    '2022_23_02_nairobi_city': 352,

    '2022_23_02_nandi': 57,

    '2022_23_02_narok': 228,

    '2022_23_02_nyamira': 0,

    '2022_23_02_nyandarua': 151,

    '2022_23_02_nyeri': 0,

    '2022_23_02_samburu': 231,

    '2022_23_02_siaya': 175,

    '2022_23_02_taita_taveta': 361,

    '2022_23_02_tana_river': 233,

    '2022_23_02_tharaka_nithi': 221,

    '2022_23_02_trans_nzoia': 169,

    '2022_23_02_turkana': 641,

    '2022_23_02_uasin_gishu': 123,

    '2022_23_02_vihiga': 0,

    '2022_23_02_wajir': 214,

    '2022_23_02_west_pokot': 0,

    '2022_23_03_bomet': 186,

    '2022_23_03_bungoma': 318,

    '2022_23_03_busia': 256,

    '2022_23_03_elgeyo_marakwet': 0,

    '2022_23_03_embu': 391,

    '2022_23_03_garissa': 192,

    '2022_23_03_homa_bay': 0,

    '2022_23_03_isiolo': 0,

    '2022_23_03_kajiado': 0,

    '2022_23_03_kakamega': 0,

    '2022_23_03_kericho': 253,

    '2022_23_03_kiambu': 218,

    '2022_23_03_kilifi': 157,

    '2022_23_03_kirinyaga': 0,

    '2022_23_03_kisii': 188,

    '2022_23_03_kisumu': 412,

    '2022_23_03_kitui': 0,

    '2022_23_03_kwale': 0,

    '2022_23_03_laikipia': 237,

    '2022_23_03_lamu': 318,

    '2022_23_03_machakos': 0,

    '2022_23_03_makueni': 0,

    '2022_23_03_mandera': 53,

    '2022_23_03_marsabit': 249,

    '2022_23_03_meru': 175,

    '2022_23_03_migori': 230,

    '2022_23_03_mombasa': 123,

    '2022_23_03_muranga': 0,

    '2022_23_03_nairobi_city': 372,

    '2022_23_03_nandi': 84,

    '2022_23_03_narok': 0,

    '2022_23_03_nyamira': 0,

    '2022_23_03_nyandarua': 301,

    '2022_23_03_nyeri': 0,

    '2022_23_03_samburu': 381,

    '2022_23_03_siaya': 241,

    '2022_23_03_taita_taveta': 359,

    '2022_23_03_tana_river': 468,

    '2022_23_03_tharaka_nithi': 353,

    '2022_23_03_trans_nzoia': 131,

    '2022_23_03_turkana': 593,

    '2022_23_03_uasin_gishu': 175,

    '2022_23_03_vihiga': 0,

    '2022_23_03_wajir': 0,

    '2022_23_03_west_pokot': 156,

    '2022_23_04_bomet': 152,

    '2022_23_04_bungoma': 279,

    '2022_23_04_busia': 0,

    '2022_23_04_elgeyo_marakwet': 0,

    '2022_23_04_embu': 369,

    '2022_23_04_garissa': 0,

    '2022_23_04_homa_bay': 0,

    '2022_23_04_isiolo': 65,

    '2022_23_04_kajiado': 0,

    '2022_23_04_kakamega': 0,

    '2022_23_04_kericho': 202,

    '2022_23_04_kiambu': 0,

    '2022_23_04_kilifi': 103,

    '2022_23_04_kirinyaga': 284,

    '2022_23_04_kisii': 175,

    '2022_23_04_kisumu': 511,

    '2022_23_04_kitui': 0,

    '2022_23_04_kwale': 0,

    '2022_23_04_laikipia': 0,

    '2022_23_04_lamu': 311,

    '2022_23_04_machakos': 320,

    '2022_23_04_makueni': 307,

    '2022_23_04_mandera': 166,

    '2022_23_04_marsabit': 371,

    '2022_23_04_meru': 163,

    '2022_23_04_migori': 278,

    '2022_23_04_mombasa': 0,

    '2022_23_04_muranga': 0,

    '2022_23_04_nairobi_city': 321,

    '2022_23_04_nandi': 0,

    '2022_23_04_narok': 0,

    '2022_23_04_nyamira': 0,

    '2022_23_04_nyandarua': 0,

    '2022_23_04_nyeri': 0,

    '2022_23_04_samburu': 716,

    '2022_23_04_siaya': 160,

    '2022_23_04_taita_taveta': 391,

    '2022_23_04_tana_river': 447,

    '2022_23_04_tharaka_nithi': 411,

    '2022_23_04_trans_nzoia': 139,

    '2022_23_04_turkana': 665,

    '2022_23_04_uasin_gishu': 222,

    '2022_23_04_vihiga': 0,

    '2022_23_04_wajir': 0,

    '2022_23_04_west_pokot': 0,

    '2023_24_01_bomet': 128,

    '2023_24_01_bungoma': 708,

    '2023_24_01_busia': 225,

    '2023_24_01_elgeyo_marakwet': 238,

    '2023_24_01_embu': 388,

    '2023_24_01_garissa': 209,

    '2023_24_01_homa_bay': 522,

    '2023_24_01_isiolo': 586,

    '2023_24_01_kajiado': 284,

    '2023_24_01_kakamega': 0,

    '2023_24_01_kericho': 218,

    '2023_24_01_kiambu': 206,

    '2023_24_01_kilifi': 108,

    '2023_24_01_kirinyaga': 225,

    '2023_24_01_kisii': 224,

    '2023_24_01_kisumu': 382,

    '2023_24_01_kitui': 404,

    '2023_24_01_kwale': 286,

    '2023_24_01_laikipia': 351,

    '2023_24_01_lamu': 243,

    '2023_24_01_machakos': 237,

    '2023_24_01_makueni': 259,

    '2023_24_01_mandera': 133,

    '2023_24_01_marsabit': 283,

    '2023_24_01_meru': 0,

    '2023_24_01_migori': 682,

    '2023_24_01_mombasa': 150,

    '2023_24_01_muranga': 0,

    '2023_24_01_nairobi_city': 325,

    '2023_24_01_nandi': 0,

    '2023_24_01_narok': 114,

    '2023_24_01_nyamira': 38,

    '2023_24_01_nyandarua': 218,

    '2023_24_01_nyeri': 216,

    '2023_24_01_samburu': 521,

    '2023_24_01_siaya': 176,

    '2023_24_01_taita_taveta': 444,

    '2023_24_01_tana_river': 336,

    '2023_24_01_tharaka_nithi': 309,

    '2023_24_01_trans_nzoia': 124,

    '2023_24_01_turkana': 461,

    '2023_24_01_uasin_gishu': 344,

    '2023_24_01_vihiga': 213,

    '2023_24_01_wajir': 226,

    '2023_24_01_west_pokot': 127,

    '2023_24_02_bomet': 162,

    '2023_24_02_bungoma': 683,

    '2023_24_02_busia': 202,

    '2023_24_02_elgeyo_marakwet': 0,

    '2023_24_02_embu': 487,

    '2023_24_02_garissa': 277,

    '2023_24_02_homa_bay': 604,

    '2023_24_02_isiolo': 504,

    '2023_24_02_kajiado': 0,

    '2023_24_02_kakamega': 152,

    '2023_24_02_kericho': 210,

    '2023_24_02_kiambu': 0,

    '2023_24_02_kilifi': 210,

    '2023_24_02_kirinyaga': 344,

    '2023_24_02_kisii': 204,

    '2023_24_02_kisumu': 430,

    '2023_24_02_kitui': 0,

    '2023_24_02_kwale': 0,

    '2023_24_02_laikipia': 528,

    '2023_24_02_lamu': 223,

    '2023_24_02_machakos': 295,

    '2023_24_02_makueni': 369,

    '2023_24_02_mandera': 95,

    '2023_24_02_marsabit': 286,

    '2023_24_02_meru': 208,

    '2023_24_02_migori': 727,

    '2023_24_02_mombasa': 173,

    '2023_24_02_muranga': 0,

    '2023_24_02_nairobi_city': 694,

    '2023_24_02_nandi': 0,

    '2023_24_02_narok': 0,

    '2023_24_02_nyamira': 0,

    '2023_24_02_nyandarua': 383,

    '2023_24_02_nyeri': 0,

    '2023_24_02_samburu': 0,

    '2023_24_02_siaya': 209,

    '2023_24_02_taita_taveta': 515,

    '2023_24_02_tana_river': 400,

    '2023_24_02_tharaka_nithi': 221,

    '2023_24_02_trans_nzoia': 160,

    '2023_24_02_turkana': 474,

    '2023_24_02_uasin_gishu': 341,

    '2023_24_02_vihiga': 0,

    '2023_24_02_wajir': 246,

    '2023_24_02_west_pokot': 0,

    '2023_24_03_bomet': 148,

    '2023_24_03_bungoma': 474,

    '2023_24_03_busia': 275,

    '2023_24_03_elgeyo_marakwet': 164,

    '2023_24_03_embu': 454,

    '2023_24_03_garissa': 269,

    '2023_24_03_homa_bay': 403,

    '2023_24_03_isiolo': 349,

    '2023_24_03_kajiado': 279,

    '2023_24_03_kakamega': 231,

    '2023_24_03_kericho': 185,

    '2023_24_03_kiambu': 170,

    '2023_24_03_kilifi': 264,

    '2023_24_03_kirinyaga': 253,

    '2023_24_03_kisii': 166,

    '2023_24_03_kisumu': 0,

    '2023_24_03_kitui': 0,

    '2023_24_03_kwale': 0,

    '2023_24_03_laikipia': 522,

    '2023_24_03_lamu': 0,

    '2023_24_03_machakos': 330,

    '2023_24_03_makueni': 228,

    '2023_24_03_mandera': 127,

    '2023_24_03_marsabit': 319,

    '2023_24_03_meru': 187,

    '2023_24_03_migori': 541,

    '2023_24_03_mombasa': 145,

    '2023_24_03_muranga': 0,

    '2023_24_03_nairobi_city': 0,

    '2023_24_03_nandi': 0,

    '2023_24_03_narok': 73,

    '2023_24_03_nyamira': 286,

    '2023_24_03_nyandarua': 274,

    '2023_24_03_nyeri': 196,

    '2023_24_03_samburu': 439,

    '2023_24_03_siaya': 185,

    '2023_24_03_taita_taveta': 344,

    '2023_24_03_tana_river': 332,

    '2023_24_03_tharaka_nithi': 289,

    '2023_24_03_trans_nzoia': 153,

    '2023_24_03_turkana': 446,

    '2023_24_03_uasin_gishu': 0,

    '2023_24_03_vihiga': 199,

    '2023_24_03_wajir': 0,

    '2023_24_03_west_pokot': 155,

    '2023_24_04_bomet': 197,

    '2023_24_04_bungoma': 651,

    '2023_24_04_busia': 356,

    '2023_24_04_elgeyo_marakwet': 230,

    '2023_24_04_embu': 424,

    '2023_24_04_garissa': 149,

    '2023_24_04_homa_bay': 480,

    '2023_24_04_isiolo': 300,

    '2023_24_04_kajiado': 342,

    '2023_24_04_kakamega': 0,

    '2023_24_04_kericho': 195,

    '2023_24_04_kiambu': 174,

    '2023_24_04_kilifi': 203,

    '2023_24_04_kirinyaga': 291,

    '2023_24_04_kisii': 163,

    '2023_24_04_kisumu': 376,

    '2023_24_04_kitui': 280,

    '2023_24_04_kwale': 581,

    '2023_24_04_laikipia': 426,

    '2023_24_04_lamu': 0,

    '2023_24_04_machakos': 302,

    '2023_24_04_makueni': 266,

    '2023_24_04_mandera': 95,

    '2023_24_04_marsabit': 362,

    '2023_24_04_meru': 177,

    '2023_24_04_migori': 702,

    '2023_24_04_mombasa': 180,

    '2023_24_04_muranga': 0,

    '2023_24_04_nairobi_city': 401,

    '2023_24_04_nandi': 0,

    '2023_24_04_narok': 74,

    '2023_24_04_nyamira': 306,

    '2023_24_04_nyandarua': 244,

    '2023_24_04_nyeri': 0,

    '2023_24_04_samburu': 472,

    '2023_24_04_siaya': 132,

    '2023_24_04_taita_taveta': 326,

    '2023_24_04_tana_river': 315,

    '2023_24_04_tharaka_nithi': 51,

    '2023_24_04_trans_nzoia': 188,

    '2023_24_04_turkana': 438,

    '2023_24_04_uasin_gishu': 0,

    '2023_24_04_vihiga': 248,

    '2023_24_04_wajir': 166,

    '2023_24_04_west_pokot': 173,

    '2024_25_01_bomet': 160,

    '2024_25_01_bungoma': 570,

    '2024_25_01_busia': 244,

    '2024_25_01_elgeyo_marakwet': 240,

    '2024_25_01_embu': 277,

    '2024_25_01_garissa': 0,

    '2024_25_01_homa_bay': 576,

    '2024_25_01_isiolo': 398,

    '2024_25_01_kajiado': 0,

    '2024_25_01_kakamega': 0,

    '2024_25_01_kericho': 198,

    '2024_25_01_kiambu': 220,

    '2024_25_01_kilifi': 258,

    '2024_25_01_kirinyaga': 0,

    '2024_25_01_kisii': 221,

    '2024_25_01_kisumu': 0,

    '2024_25_01_kitui': 291,

    '2024_25_01_kwale': 376,

    '2024_25_01_laikipia': 403,

    '2024_25_01_lamu': 183,

    '2024_25_01_machakos': 273,

    '2024_25_01_makueni': 237,

    '2024_25_01_mandera': 312,

    '2024_25_01_marsabit': 357,

    '2024_25_01_meru': 198,

    '2024_25_01_migori': 768,

    '2024_25_01_mombasa': 134,

    '2024_25_01_muranga': 0,

    '2024_25_01_nairobi_city': 751,

    '2024_25_01_nandi': 0,

    '2024_25_01_narok': 0,

    '2024_25_01_nyamira': 316,

    '2024_25_01_nyandarua': 277,

    '2024_25_01_nyeri': 0,

    '2024_25_01_samburu': 501,

    '2024_25_01_siaya': 163,

    '2024_25_01_taita_taveta': 560,

    '2024_25_01_tana_river': 283,

    '2024_25_01_tharaka_nithi': 0,

    '2024_25_01_trans_nzoia': 244,

    '2024_25_01_turkana': 0,

    '2024_25_01_uasin_gishu': 206,

    '2024_25_01_vihiga': 157,

    '2024_25_01_wajir': 197,

    '2024_25_01_west_pokot': 99,

    '2024_25_02_bomet': 124,

    '2024_25_02_bungoma': 438,

    '2024_25_02_busia': 224,

    '2024_25_02_elgeyo_marakwet': 151,

    '2024_25_02_embu': 225,

    '2024_25_02_garissa': 265,

    '2024_25_02_homa_bay': 525,

    '2024_25_02_isiolo': 311,

    '2024_25_02_kajiado': 218,

    '2024_25_02_kakamega': 0,

    '2024_25_02_kericho': 149,

    '2024_25_02_kiambu': 217,

    '2024_25_02_kilifi': 120,

    '2024_25_02_kirinyaga': 239,

    '2024_25_02_kisii': 193,

    '2024_25_02_kisumu': 412,

    '2024_25_02_kitui': 268,

    '2024_25_02_kwale': 177,

    '2024_25_02_laikipia': 433,

    '2024_25_02_lamu': 158,

    '2024_25_02_machakos': 217,

    '2024_25_02_makueni': 255,

    '2024_25_02_mandera': 279,

    '2024_25_02_marsabit': 352,

    '2024_25_02_meru': 0,

    '2024_25_02_migori': 974,

    '2024_25_02_mombasa': 194,

    '2024_25_02_muranga': 0,

    '2024_25_02_nairobi_city': 280,

    '2024_25_02_nandi': 0,

    '2024_25_02_narok': 0,

    '2024_25_02_nyamira': 301,

    '2024_25_02_nyandarua': 180,

    '2024_25_02_nyeri': 0,

    '2024_25_02_samburu': 385,

    '2024_25_02_siaya': 186,

    '2024_25_02_taita_taveta': 592,

    '2024_25_02_tana_river': 164,

    '2024_25_02_tharaka_nithi': 0,

    '2024_25_02_trans_nzoia': 149,

    '2024_25_02_turkana': 0,

    '2024_25_02_uasin_gishu': 155,

    '2024_25_02_vihiga': 225,

    '2024_25_02_wajir': 159,

    '2024_25_02_west_pokot': 152,

    '2024_25_03_bomet': 142,

    '2024_25_03_bungoma': 457,

    '2024_25_03_busia': 199,

    '2024_25_03_elgeyo_marakwet': 187,

    '2024_25_03_embu': 182,

    '2024_25_03_garissa': 209,

    '2024_25_03_homa_bay': 487,

    '2024_25_03_isiolo': 369,

    '2024_25_03_kajiado': 275,

    '2024_25_03_kakamega': 199,

    '2024_25_03_kericho': 159,

    '2024_25_03_kiambu': 188,

    '2024_25_03_kilifi': 184,

    '2024_25_03_kirinyaga': 277,

    '2024_25_03_kisii': 163,

    '2024_25_03_kisumu': 380,

    '2024_25_03_kitui': 345,

    '2024_25_03_kwale': 220,

    '2024_25_03_laikipia': 433,

    '2024_25_03_lamu': 0,

    '2024_25_03_machakos': 237,

    '2024_25_03_makueni': 204,

    '2024_25_03_mandera': 194,

    '2024_25_03_marsabit': 269,

    '2024_25_03_meru': 170,

    '2024_25_03_migori': 928,

    '2024_25_03_mombasa': 145,

    '2024_25_03_muranga': 0,

    '2024_25_03_nairobi_city': 412,

    '2024_25_03_nandi': 0,

    '2024_25_03_narok': 0,

    '2024_25_03_nyamira': 264,

    '2024_25_03_nyandarua': 267,

    '2024_25_03_nyeri': 195,

    '2024_25_03_samburu': 361,

    '2024_25_03_siaya': 161,

    '2024_25_03_taita_taveta': 166,

    '2024_25_03_tana_river': 332,

    '2024_25_03_tharaka_nithi': 0,

    '2024_25_03_trans_nzoia': 157,

    '2024_25_03_turkana': 561,

    '2024_25_03_uasin_gishu': 172,

    '2024_25_03_vihiga': 0,

    '2024_25_03_wajir': 212,

    '2024_25_03_west_pokot': 0,
}

def get_csv_line_count(file_path):
    """Get line count of a CSV file."""
    if not os.path.exists(file_path):
        return 0
    
    with open(file_path, 'r', encoding='utf-8') as f:
        return sum(1 for line in f if line.strip())  # Count non-empty lines

def test_program_csv_extraction():
    """Test that program.py generates CSVs with expected line counts."""
    program_dir = Path('program')
    
    if not program_dir.exists():
        pytest.skip("Program directory does not exist. Run 'python program.py --all' first.")
    
    missing_files = []
    incorrect_counts = []
    
    for key, expected_count in EXPECTED_LINE_COUNTS.items():
        # Parse key format: year_quarter_county
        parts = key.split('_')
        if len(parts) >= 3:
            year = f"{parts[0]}_{parts[1]}"  # e.g., "2019_20"
            quarter = parts[2]               # e.g., "01"
            county = '_'.join(parts[3:])     # e.g., "baringo" or "elgeyo_marakwet"
            
            csv_path = program_dir / year / quarter / 'county' / f'{county}_programme_table.csv'
            
            if not csv_path.exists():
                missing_files.append(str(csv_path))
                continue
                
            actual_count = get_csv_line_count(csv_path)
            if actual_count != expected_count:
                incorrect_counts.append({
                    'file': str(csv_path),
                    'expected': expected_count,
                    'actual': actual_count
                })
    
    # Report results
    if missing_files:
        pytest.fail(f"Missing CSV files:\n" + '\n'.join(missing_files))
    
    if incorrect_counts:
        error_msg = "Incorrect line counts:\n"
        for item in incorrect_counts:
            error_msg += f"  {item['file']}: expected {item['expected']}, got {item['actual']}\n"
        pytest.fail(error_msg)

def test_program_extraction_runs():
    """Test that program.py can run without errors on a sample."""
    # This test ensures the basic functionality works
    result = subprocess.run(['python', 'program.py', '--help'], 
                          capture_output=True, text=True)
    assert result.returncode == 0, f"program.py --help failed: {result.stderr}"

if __name__ == '__main__':
    # Run tests directly
    test_program_extraction_runs()
    test_program_csv_extraction()
    print("All tests passed!")