from flask import Flask, render_template, request, jsonify
import re
import tensorflow as tf

app = Flask(__name__)

# Load your trained model
model = tf.keras.models.load_model('cobol_python_translation_model1.h5')

def preprocess_cobol_code(code):
    # Remove unwanted code and perform preprocessing
    # For example, remove comments and extra spaces
    code = re.sub(r'\*.*', '', code)  # Remove comments
    code = re.sub(r'\n\s*\n', '\n', code)  # Remove empty lines
    code = code.strip()  # Remove leading and trailing spaces
    return code

@app.route('/')
def index():
    return render_template('index.html')

@app.route('/convert', methods=['POST'])
def convert():
    try:
        # Get data from the form
        legacy_code = request.form['legacy_code']
        legacy_language = request.form['legacy_language']
        new_language = request.form['new_language']

        # Preprocess the COBOL code
        preprocessed_code = preprocess_cobol_code(legacy_code)

        # Translate the preprocessed code using your trained model
        # Assuming your model expects a specific input shape or format
        # Convert the code to a format suitable for the model input
        input_data = tf.constant([preprocessed_code])  # Example, adjust as necessary

        # Translate the code using the model
        predictions = model.predict(input_data)

        # Convert model output to a string format
        new_code = ''.join(predictions)

        return jsonify({'new_code': new_code})
    except Exception as e:
        return str(e), 500

if __name__ == '__main__':
    app.run(debug=True)
