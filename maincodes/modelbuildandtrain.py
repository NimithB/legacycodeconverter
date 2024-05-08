import numpy as np
import pickle
from tensorflow.keras.models import Model
from tensorflow.keras.layers import Input, LSTM, Dense

# Load the dataset
with open('cobol_python_data.pkl', 'rb') as f:
    cobol_train, cobol_test, python_train, python_test = pickle.load(f)

# Combine Cobol and Python data
cobol_texts = cobol_train + cobol_test
python_texts = python_train + python_test

# Ensure each item in cobol_texts is a string
cobol_texts = [''.join(cobol_text) for cobol_text in cobol_texts]

# Ensure each item in python_texts is a string
python_texts = [''.join(python_text) for python_text in python_texts]

# Define constants
max_cobol_length = max(len(seq) for seq in cobol_texts)
max_python_length = max(len(seq) for seq in python_texts)

# Define token sets
cobol_characters = sorted(set(''.join(cobol_texts)))
python_characters = sorted(set(''.join(python_texts)))
num_cobol_tokens = len(cobol_characters)
num_python_tokens = len(python_characters)

# Token indices dictionaries
cobol_token_index = dict((char, i) for i, char in enumerate(cobol_characters))
python_token_index = dict((char, i) for i, char in enumerate(python_characters))

# Vectorize the data
encoder_input_data = np.zeros((len(cobol_texts), max_cobol_length, num_cobol_tokens), dtype='float32')
decoder_input_data = np.zeros((len(cobol_texts), max_python_length, num_python_tokens), dtype='float32')
decoder_target_data = np.zeros((len(cobol_texts), max_python_length, num_python_tokens), dtype='float32')

for i, (cobol_text, python_text) in enumerate(zip(cobol_texts, python_texts)):
    for t, char in enumerate(cobol_text):
        encoder_input_data[i, t, cobol_token_index[char]] = 1.0
    for t, char in enumerate(python_text):
        decoder_input_data[i, t, python_token_index[char]] = 1.0
        if t > 0:
            decoder_target_data[i, t - 1, python_token_index[char]] = 1.0

# Define constants
batch_size = 64  # Batch size for training
epochs = 100  # Number of epochs to train for
latent_dim = 256  # Latent dimensionality of the encoding space

# Define an input sequence and process it
encoder_inputs = Input(shape=(None, num_cobol_tokens))
encoder = LSTM(latent_dim, return_state=True)
encoder_outputs, state_h, state_c = encoder(encoder_inputs)
encoder_states = [state_h, state_c]

# Set up the decoder, using `encoder_states` as initial state
decoder_inputs = Input(shape=(None, num_python_tokens))
decoder_lstm = LSTM(latent_dim, return_sequences=True, return_state=True)
decoder_outputs, _, _ = decoder_lstm(decoder_inputs, initial_state=encoder_states)
decoder_dense = Dense(num_python_tokens, activation='softmax')
decoder_outputs = decoder_dense(decoder_outputs)

# Define the model that will turn `encoder_input_data` & `decoder_input_data` into `decoder_target_data`
model = Model([encoder_inputs, decoder_inputs], decoder_outputs)

# Compile the model
model.compile(optimizer='rmsprop', loss='categorical_crossentropy', metrics=['accuracy'])


model.fit([encoder_input_data, decoder_input_data], decoder_target_data,
          batch_size=batch_size,
          epochs=epochs,
          validation_split=0.2)

model.save('cobol_python_translation_model1.h5')
