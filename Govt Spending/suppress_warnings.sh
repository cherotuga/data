#!/bin/bash
# Set environment variables to suppress common data science warnings

export TOKENIZERS_PARALLELISM=false
export MKL_NUM_THREADS=1

echo "✅ Environment variables set to suppress warnings:"
echo "   TOKENIZERS_PARALLELISM=$TOKENIZERS_PARALLELISM"
echo "   MKL_NUM_THREADS=$MKL_NUM_THREADS"
echo ""
echo "To make these permanent, add the following to your ~/.bashrc or ~/.zshrc:"
echo "   export TOKENIZERS_PARALLELISM=false"
echo "   export MKL_NUM_THREADS=1"