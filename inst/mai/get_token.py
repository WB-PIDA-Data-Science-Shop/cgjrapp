"""Print a mAI desktop access token to stdout, for cgjrapp (R) to read.

Usage: python get_token.py [DEV|QA|PROD]   (default DEV)

Run inside a uv project where itsai-platform is installed; see
CGJR_MAI_PYTHON_PROJECT in R/utils_mai.R. Desktop access is for proof of
concept and local development only.
"""
import sys

from itsai.platform.authentication import DesktopToken

env = sys.argv[1].upper() if len(sys.argv) > 1 else "DEV"
# itsai-platform names the DEV desktop scope DEV_DESKTOP; QA and PROD are as is
sys.stdout.write(DesktopToken().get_token(env="DEV_DESKTOP" if env == "DEV" else env))
