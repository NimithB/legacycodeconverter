import re
import string
from nltk.corpus import stopwords
from nltk.stem import SnowballStemmer
from sklearn.feature_extraction.text import TfidfVectorizer
import pandas as pd
import nltk
nltk.download('punkt')
nltk.download('stopwords')

text = """
IDENTIFICATION DIVISION.
PROGRAM-ID. SAMPLE-PROGRAM.
DATA DIVISION.
WORKING-STORAGE SECTION.
01 WS-VAR1 PIC X(10).
01 WS-VAR2 PIC 9(5).
PROCEDURE DIVISION.
BEGIN.
    MOVE 'Hello, world!' TO WS-VAR1.
    PERFORM DISPLAY-VAR1 UNTIL WS-VAR2 = 0.
DISPLAY-VAR1.
    DISPLAY WS-VAR1.
    ADD 1 TO WS-VAR2.
STOP RUN.
"""

def tokenize(text):
    # Remove punctuation
    text = re.sub(r'[^\w\s]', '', text)

    # Convert to lowercase
    text = text.lower()

    # Tokenize
    tokens = nltk.word_tokenize(text)

    # Remove stopwords
    stop_words = set(stopwords.words('english'))
    tokens = [token for token in tokens if token not in stop_words]

    return tokens

def stem_and_vectorize(cobol_tokens, vectorizer=None):
    """
    Stems and vectorizes the given COBOL tokens.
    """
    # Convert to lowercase
    cobol_tokens = [token.lower() for token in cobol_tokens]

    # Remove punctuation
    cobol_tokens = [token.translate(str.maketrans('', '', string.punctuation)) for token in cobol_tokens]

    # Remove stopwords
    stop_words = set(stopwords.words('english'))
    cobol_tokens = [token for token in cobol_tokens if token not in stop_words]

    # Stemming
    stemmer = SnowballStemmer('english')
    cobol_tokens = [stemmer.stem(token) for token in cobol_tokens]

    if vectorizer is None:
        # Create a new vectorizer
        vectorizer = TfidfVectorizer()

    # Vectorize the COBOL tokens
    cobol_vectors = vectorizer.fit_transform(cobol_tokens)

    return cobol_vectors, vectorizer

def main():
    # Tokenize the input text
    cobol_tokens = tokenize(text)

    # Preprocess the tokens and vectorize them
    cobol_vectors, vectorizer = stem_and_vectorize(cobol_tokens)

    # Export the resulting matrix to a file
    df = pd.DataFrame(cobol_vectors.toarray(), columns=vectorizer.get_feature_names_out())
    df.to_csv('cobol_vectors1.csv', index=False)

if __name__ == "__main__":
    main()