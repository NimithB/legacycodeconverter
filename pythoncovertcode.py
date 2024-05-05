import re
import string
from nltk.corpus import stopwords
from nltk.tokenize import word_tokenize
from nltk.stem import SnowballStemmer

code = """import time

def main():
    var1 = 'Hello, world!'
    var2 = 0

    while var2 < 5:
        print(var1)
        var2 += 1

if __name__ == '__main__':
    main()
"""

def tokenize_python(code):
    # Remove comments and strings
    code = re.sub(r'#.*?$', '', code, flags=re.MULTILINE)
    code = re.sub(r'"[^"]*"|\'[^\']*\'', '', code)

    # Convert to lowercase
    code = code.lower()

    # Tokenize
    tokens = word_tokenize(code)

    # Remove punctuation
    tokens = [token.translate(str.maketrans('', '', string.punctuation)) for token in tokens]

    # Remove stopwords
    stop_words = set(stopwords.words('english'))
    tokens = [token for token in tokens if token not in stop_words]

    # Stemming
    stemmer = SnowballStemmer('english')
    stemmed_tokens = [stemmer.stem(token) for token in tokens]

    return stemmed_tokens

# Define stemmed_tokens
stemmed_tokens = tokenize_python(code)

# Print stemmed_tokens
print(stemmed_tokens)