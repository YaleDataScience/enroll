'''
Created Jan 2015

@author: henryre
'''
import argparse
import csv
from nltk.tokenize import RegexpTokenizer
from nltk.corpus import stopwords
from nltk.stem.porter import PorterStemmer
from nltk.stem import RegexpStemmer

# Remove non-standard characters from string
def stripSpecial(s):
    return "".join(c for c in s if ord(c)<128)

# Basic text analyzer
def analyze(text, stop, stem, wstem):
    # Set utilities
    if stop:
        stopeng = set(stopwords.words('english'))
    if wstem:
        stemmer = RegexpStemmer('ing$|s$|e$', min=4)
    if stem:
        stemmer = PorterStemmer()
    tok = RegexpTokenizer(r'\w+')
    # Remove weird characters
    text = stripSpecial(text)
    # Tokenize and lowercase
    text = tok.tokenize(text.lower())
    # Remove stopwords if flagged
    if stop:
        text = [w for w in text if w not in stopeng]
    # Stem if flagged
    if (stem or wstem):
        text = [stemmer.stem(w) for w in text]
    return ' '.join(text)

# Main module
def agg(pcomments, pnames, pdemand, pout, stop, stem, wstem):
    # Create dictionary mapping course id to comment text
    commentdict = {}
    with open(pcomments, 'rb') as fcomments:
        reader = csv.reader(fcomments, delimiter=',')
        # Skip header
        next(reader, None)
        # Iterate over every review record
        for row in reader:
            courseid = row[1]
            text = row[3]
            # Preprocess text
            text = analyze(text, stop, stem, wstem)
            # Add or append
            if courseid not in commentdict.keys():
                commentdict[courseid] = text
            else:
                commentdict[courseid] = commentdict[courseid] + ' ' + text
    # Create dictionary mapping course name to course id
    coursedict = {}
    with open(pnames, 'rb') as fnames:
        reader = csv.reader(fnames, delimiter=',')
        # Skip header
        next(reader, None)
        # Iterate over every course
        for row in reader:
            courseid = row[1]
            subj = row[2]
            num = row[3]
            if subj not in coursedict.keys():
                coursedict[subj] = {}
            if num not in coursedict[subj].keys():
                coursedict[subj][num] = courseid
    # Create dictionary mapping course id to demand
    demanddict = {}
    with open(pdemand, 'rb') as fdemand:
        reader = csv.reader(fdemand, delimiter='\t')
        for row in reader:
            # Find first course name
            name = row[0]
            name = name.split('/')[0]
            split = name.split(' ')
            subj = split[0]
            num = split[1]
            if subj in coursedict.keys():
                if num in coursedict[subj].keys():
                    # Use course dict to translate name to ID
                    cid = coursedict[subj][num]
                    # Store final demand
                    demanddict[cid] = row[len(row)-1]
    
    # Write out
    with open(pout, 'w+') as f:
        writer = csv.writer(f, delimiter='\t')
        # Only write courses with both demand and comments
        common = set(demanddict.keys()).intersection(set(commentdict.keys()))
        for c in common:
            writer.writerow([c, demanddict[c], commentdict[c]])            
            

if __name__ == '__main__':
    parser = argparse.ArgumentParser(description='Aggregate OCS data')
    parser.add_argument('comments', type=str, help='Path to course comment TSV')
    parser.add_argument('names', type=str, help='Path to course name TSV')
    parser.add_argument('demand', type=str, help='Path to course demand TSV')
    parser.add_argument('output', type=str, help='Path to output file')
    parser.add_argument('--stop', dest='stop', action='store_const', const=True, default=False, help="Remove standard English stopwords.")
    parser.add_argument('--stem', dest='stem', action='store_const', const=True, default=False, help="Use the Porter stemming algorithm.")
    parser.add_argument('--wstem', dest='wstem', action='store_const', const=True, default=False, help="Use a weak stemming algorithm.")
    args = parser.parse_args()
    
    agg(args.comments, args.names, args.demand, args.output, args.stop, args.stem, args.wstem)