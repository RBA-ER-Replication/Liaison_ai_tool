"""Utility functions and classes"""

import pandas as pd
import urllib
import traceback
import os
import re
from pathlib import Path
from pytz import timezone
from datetime import datetime
import numpy as np
import re
from functools import partial
from collections import defaultdict
import docx
from docx.document import Document
from docx.text.paragraph import Paragraph
from docx.table import _Cell, Table
from docx.oxml.table import CT_Tbl
from docx.oxml.text.paragraph import CT_P
from typing import List, Set, Dict, Tuple, Union
import io
import warnings
from tqdm import tqdm

    
class DocXNote:
    """Parses the content of a note into paragraphs, tables, etc."""

    def __init__(self):
        self.file_contents = []


    def parse(self, filepath):
        """
        Parses a DOCX document, iterates over Paragraph or Table and retrieves the
        texts and styles for each.  Calls __iter_block_items.

        The text data is saved in the liaison_file.file_contents
        """
        source_stream = None
        with open(filepath, 'rb') as f:
            source_stream = io.BytesIO(f.read())
        document = docx.Document(source_stream)
        source_stream.close()

        for block in DocXNote._iter_block_items(document):
            if isinstance(block,Table):
                text = DocXNote._table_to_text(block)
                self._add_contents("Table","n/a",text)
            else:
                self._add_contents("Paragraph", block.style.name, block.text)

        return pd.DataFrame.from_records(self.file_contents)

    def _add_contents(self, element, style, text):
        seq_id = len(self.file_contents) + 1
        self.file_contents.append({
           "seq_id":seq_id,
           "element":element,
           "style":style,
           "text":text
        })

    @staticmethod
    def _table_to_text(block):
        table_texts = []
        for row in block.rows:
            row_data = []
            for cell in row.cells:
                for para in cell.paragraphs:
                    row_data.append(para.text)
            table_texts.append("\t".join(row_data))
        text = "\n".join(table_texts)

    @staticmethod
    def _iter_block_items(parent):
        """
        Generate a reference to each paragraph and table child within *parent*,
        in document order. Each returned value is an instance of either Table or
        Paragraph. *parent* would most commonly be a reference to a main
        Document object, but also works for a _Cell object, which itself can
        contain paragraphs and tables.
        """
        if isinstance(parent, Document):
            parent_elm = parent.element.body
            # print(parent_elm.xml)
        elif isinstance(parent, _Cell):
            parent_elm = parent._tc
        else:
            raise ValueError("something's not right")

        for child in parent_elm.iterchildren():
            if isinstance(child, CT_P):
                yield Paragraph(child, parent)
            elif isinstance(child, CT_Tbl):
                yield Table(child, parent)



def make_list_regex(str_list:List[str], start=r'\b', end=r'\b', group:bool=True) -> str:
    """
    Makes a list of words into a single regex that identifies if any are present.
    
    Parameters
    ------------
    str_list: The set of strings to search for
    startswith: Should the pattern only match when the text startswith one of the words in the list
    group: Should the regex be made into a group
    
    """
    regex_terms = [start+re.escape(s)+end for s in str_list]   
    regex = '|'.join(regex_terms)
    if group:
        regex = '('+regex+')'
    return regex

# +
def is_title(words, min_cap_len = 4):
    """Check if a list of words is title cased."""
    for word in words:
        if len(word) >= min_cap_len and not word[0].isdigit() and not word[0].isupper():
            return False
    return True

def prop_title_cased(words):
    """Check the proportion of words are title cased."""
    skip_shorter_than = 4
    words = [w for w in words if len(w)>= skip_shorter_than]
    if len(words) == 0:
        return -1
    count = sum([(w[0].isupper()) for w in words])
    return count/len(words)

non_alpha = re.compile('[^a-zA-Z\s]')
def clean_words(text):
    """Check how many words are using only the standard alphabetical symbols."""
    words = re.sub(non_alpha, ' ',text).split()
    words = [w for w in words if len(w)>0]
    return words


def construct_text_features(pars, add_extra_cols=False):
    """
    Enrich the pars df with additional columns used to determine if a given section of text is a heading.
    Note modifies the text 
    
    
    """
    required_columns = [
        'text',
        'style', 
        'element'
    ]
    for col in required_columns:
        if col not in pars.columns:
            raise ValueError(f'pars must include column {col}')
    

    pars['heading_style'] = (pars['style'].str.contains('Title')|pars['style'].str.contains('Heading'))

    pars['has_question'] = pars['text'].str.contains(r'\?')
    pars['num_colons'] = pars['text'].str.count(':')
    pars['star_start'] = pars['text'].str.startswith('*')
    pars['first_letter_upper'] = pars['text'].str[0].str.isupper()
    pars['ends_full_stop'] = pars['text'].str.endswith('.')
    pars['ends_question'] = pars['text'].str.endswith('?')
    pars['words'] = pars['text'].map(clean_words)
    pars['nwords'] = pars['words'].map(len)
    pars['nunique_words'] = pars['words'].map(lambda words: len(set(w.lower() for w in words)))
    pars['prop_title_case'] = pars['words'].map(prop_title_cased)

    
    phone_regex = make_list_regex(['phone','ph','tel','telephone'],group=False)

    qtrs_regex = make_list_regex([
        'quater','qtr','jan','feb','mar','apr','may',
        'jun','jul','aug','sep','sept','oct','nov','dec'
    ],group=False)

    tb_regex_require_colon = make_list_regex([
        'appendix','contact','date','interviewed',
        'figure','chart','photo','company','source',
        'see:','meeting with:','table:',
    ],start=r'^',end=r'',group=False)
    tb_regex_require_colon = '|'.join([phone_regex,tb_regex_require_colon])

    tb_regex = make_list_regex([
        'person(s) interviewed',
        'meeting with',
        'table',
        'figure',
        'graph',
        'chart',
        'see',
    ],start=r'^',group=False)

    question_start_regex = make_list_regex([
        'How',
        'What',
        'Are',
        'Have',
        'Has',
        'Is',
        'Do',
        'Would',
        'Which',
        'Does',
        'Will',
        'Why'
    ],start=r'^',group=False)

    lias_regex = make_list_regex([
        'likert',
        'record of interview',
        'economic analysis',
        'Regional and Industry Analysis',
        'Regional & Industry Analysis',
        'Senior Representative',
        'Diary Note',
        'South Australian Office',
        'Victorian Office',
        'Victorian State Office',
        'Western Australian Office',
        'Queensland Office',
        'Senior Research Assistant',
    ],group=False)

    months = r'(january|february|march|april|may|june|july|august|september|october|november|december)'
    date_pattern = r'\s*\d{1,2}[\s,]+'+months+r'[\s,]+\d{2,4}\s*'

    # more feature construction
    lias = pars['text'].str.contains(lias_regex,regex=True,case=False)
    lower = pars['text'].str.lower()
    table = pars['element'] == 'Table'
    is_date = pars['text'].str.match(date_pattern, flags=re.IGNORECASE)
    tablelike = (
        (pars['text'].str.contains(tb_regex_require_colon,regex=True, case=False)&(pars['num_colons']>0))|
        (pars['text'].str.contains(tb_regex,regex=True,case=False))|
        (lower.str.count(qtrs_regex) >= 5)
    )
    question_start = pars['text'].str.contains(question_start_regex,regex=True,case=True)
    question = question_start | (pars['ends_question'])
    table = table|tablelike
    economist = (pars['text'].str.contains(r'\bEconomist\b',regex=True, case=False)) & (pars['nwords']<6)
    if add_extra_cols:
        pars['is_economist'] = economist
        pars['is_date'] = is_date
        pars['is_table'] = table
        pars['is_question'] = question
    return lias, is_date, economist, table, question


def pd_left_join_all(frames:List[pd.DataFrame], on:Union[str, List[str]]) -> pd.DataFrame:
    """
    Performs a left join on multiple dataframes using the same key(s).
    
    This function takes a list of dataframes and performs a left join using the same key(s) for all joins. 
    If less than two frames are passed, a ValueError is raised.

    Args:
        frames (list): A list of pandas DataFrame objects to be joined.
        on (str or list): Column(s) to join on. Can be a single column name, or a list of names.

    Returns:
        pandas.DataFrame: The resulting dataframe after performing the left joins.

    Raises:
        ValueError: If less than two frames are passed.

    Example:

        ```python
        >>> df1 = pd.DataFrame({'A': ['A0', 'A1', 'A2', 'A3'], 'B': ['B0', 'B1', 'B2', 'B3']})
        >>> df2 = pd.DataFrame({'A': ['A0', 'A1', 'A3'], 'C': ['C0', 'C1', 'C3']})
        >>> df3 = pd.DataFrame({'A': ['A1', 'A0', 'A2', 'A3'], 'D': ['D1', 'D0', 'D2', 'D3']})
        >>> frames = [df1, df2, df3]
        >>> result = pd_left_join_all(frames, 'A')
        >>> print(result)
           A   B   C   D
        0  A0  B0  C0  D0
        1  A1  B1  C1  D1
        2  A2  B2  NaN  D2
        3  A3  B3  C3  D3
        ```
    """
    if len(frames)<2:
        raise ValueError('Must pass at least 2 frames to join.')
    joined = frames[0]
    for right in frames[1:]:
        joined = pd.merge(joined, right, on=on, how='left')
    return joined


def identify_headings(pars, primarily_use_style=False, add_extra_cols=False):
    """
    Enrich the pars table with heuristics predicting if a paragraph is a heading or not.
    
    Parameters
    ------------
    pars: pd.DataFrame
        The paragraphs to be tagged
    
    primarily_use_style: bool
        Post 2016 the use of heading style has been much more consistent - so this flag switches the behaviour
        of the heuristic to primarly use header style to distinguish body content from headers.
    """
    starting_columns = list(pars.columns)
    created_columns = ['category','nwords']
    keep_columns = starting_columns + created_columns
    
    lias, is_date, economist, table, question = construct_text_features(pars, add_extra_cols)


    # apply hueristics
    info = lias|is_date|(pars['seq_id']==1)|economist
    
    if primarily_use_style:
        is_heading = (~info)&(~table)&pars['heading_style']&(pars['nwords']<30)
        not_heading = (~info)&(~table)&(~is_heading)
        
    else:
        is_heading = (
            (~info)&
            (~table)&
            (
                (pars['heading_style'])|
                question|
                (pars['prop_title_case']>=.99)|
                (pars['first_letter_upper']&(~pars['ends_full_stop'])&(pars['nwords']<=10))
            )
        )
        # genuine content
        not_heading = (
            (~info)&
            (~table)&
            (~is_heading)
        )
    
    pars['category'] = 'UNK'
    pars.loc[is_heading,'category'] = 'HEAD'
    pars.loc[not_heading,'category'] = 'BODY'
    pars.loc[table,'category']='TABLE'
    pars.loc[info,'category'] = 'INFO'
    
    if not add_extra_cols:
        drop_columns = set(pars.columns).difference(keep_columns)
        pars.drop(columns=drop_columns, inplace=True)


def clean_content(content):
    """Remove duplicate, empty and outdated rows."""

    # remove duplicate spaces
    content['text'] = content['text'].replace(r'\s+',' ', regex=True).str.strip()

    no_text = content['text'].isnull() | (content['text'].str.len() == 0)
    print(f'Removing {no_text.sum()} rows with no text')
    content = content[~no_text].copy()
    n = len(content)
    if 'rev_id' in content.columns:
        content = content.sort_values(by='rev_id').drop_duplicates(['file_id','seq_id'], keep='last').sort_index()
    else:
        content = content.drop_duplicates(['file_id','seq_id'], keep='last')

    dropped = n - len(content)
    print(f'Removed {dropped} duplicated/outdated rows')
    return content


def detect_content_type(content, primarily_use_style=False):
    """Apply heuristics to get content type."""
    identify_headings(content, primarily_use_style, False)
    #drop_cols = set(content.columns).difference(keep_cols)
    return content


def add_last_heading_column(content):
    """Add a new column which contains the value of the previous heading (if any) within that file"""
    content['last_heading'] = np.nan
    frames = []
    for key, data in content.groupby('file_id'):
        data = data.sort_values(by=['seq_id'])
        is_head = data['category']=='HEAD'
        data.loc[is_head,'last_heading'] = data.loc[is_head,'text']
        data['last_heading'] = data['last_heading'].fillna(method='ffill')
        frames.append(data)
    result = pd.concat(frames)
    return result


def left_only(table1, table2, key, key_out=None):
    """
    Return the keys that are only in the left table as an array.

    Parameters
    --------------------
    table1: pd.DataFrame
        The left table
    table2: pd.DataFrame
        The right table
    key: str or list[str]
        The column(s) to join the tables on
    key_out: str (optional)
        The column in the left table to return for rows where no matching record found on the right.

    """
    if not isinstance(key, list):
        key = [key]
    left_cols = key[:]
    if key_out is not None:
        if isinstance(key_out, list):
            raise ValueError('Key out must be a single column.')
        left_cols.append(key_out)

    # check to see types match
    for column in key:
        type1, type2 = table1[column].dtype, table2[column].dtype
        if type1!=type2:
            raise ValueError(f'Attempting to join on column {column} but types dont match ({type1} vs {type2})')


    lo = pd.merge(table1[left_cols],table2, how='left',on=key, indicator=True)
    if key_out is None:
        if len(key) > 1:
            raise ValueError('Multi-column key used and no value set for key_out')
        key_out = key[0]
    lo = lo.loc[lo['_merge']=='left_only',key_out].copy().values
    return lo

def vstack(df1, df2, reset_index=False):
    """Vertically join two pandas DataFrames with identical columns."""
    if set(df1.columns) != set(df2.columns):
        raise ValueError(f'Columns do not match:{df1.columns} vs {df2.columns}')
    result = pd.concat([df1, df2], axis=0)
    if reset_index:
        result.reset_index(drop=True, inplace=True)
    return result


def current_datetime():
    """Return the current datetime with timezone."""
    sydney_tz = timezone('Australia/Sydney')
    now = datetime.now(sydney_tz)
    return now


def error_str(ex):
    """String representation of a python Exception."""
    return ''.join(traceback.format_exception(etype=type(ex), value=ex, tb=ex.__traceback__))

def remove_suffix(columns, suffix):
    """Return a list of columns with the given suffix removed (if present)."""
    l = len(suffix)
    result = []
    for c in columns:
        if c.endswith(suffix):
            result.append(c[:-l])
        else:
            result.append(c)
    return result

def fix_datetime_format(row):
    """
    Use preferred date formats for SQLServer
    https://stackoverflow.com/questions/14119133/conversion-failed-when-converting-date-and-or-time-from-character-string-while-i
    """
    new_col='_s'
    value = row[new_col]
    if pd.isnull(value):
        return None # None works, but NaT does not!
    if row['changed'] in ['ModifyDateTime']:
        return pd.to_datetime(value).strftime('%Y-%m-%dT%H:%M:%S')
    elif row['changed'] in ['ContactDate','FollowUpDate']:
        return pd.to_datetime(value).strftime('%Y%m%d')

    return row[new_col]

def find_target_num(text, target, model, answer = True):
    """
    Retrieval of target values using QA model
    """
    QA_input = {
        'question': f'What is the rate associated with {target} change?',
        'context': text
    }
    res = model(QA_input)
    if answer:
        return res['answer']
    else:
        return res
    
def find_target_dir(text, target, model, answer = True):
    '''Determine direction of target change using a 0shot text classifier'''
    candidate_labels = [f'{target} increase', f'{target} decrease', f'no changes to {target}']
    res = model(text, candidate_labels)
    if answer:
        return res['labels'][0]
    else:
        return res
    
def sign_from_dir(text, target):
    ''' Converting direction string to sign'''
    if text == f'{target} increase':
        return 1
    elif text == f'{target} decrease':
        return -1
    else:
        return 0    
    
def numextract_pre_process(input_content, targets = ['price','prices']):
    '''
    Pre-processing of ria_db in preparation of numerical extraction.
    '''
    
    ## Filter for sentences containing target list, filter date
    content = input_content.copy()
    content['textl'] = content['text'].str.lower()
    num_regex = make_list_regex(targets,group=False)
    numext = content.loc[content['textl'].str.contains(num_regex, regex=True)].copy()
    
    ## Filter for percentages
    percent_keywords = ['percent','per cent', '%']
    percent_regex = make_list_regex(percent_keywords,group=False)
    numext_percent = numext.loc[numext['textl'].str.contains(percent_regex, regex=True)].copy()
    
    ## Filter for sentences containing numbers
    number_pattern = r'\s(?P<num>\d{1,2}(?:\.\d)?)(?P<frac>[\u2150-\u215E\u00BC-\u00BE])?\s'
    range_pattern =r'\s(?P<num>\d{1,2}(?:\.\d)?)(?P<frac>[\u2150-\u215E\u00BC-\u00BE])?'+r'\s?(?:[\-\-\–]{1}|and)\s?'+r'(?P<num2>\d{1,2}(?:\.\d)?)(?P<frac2>[\u2150-\u215E\u00BC-\u00BE])?\s'
    with warnings.catch_warnings():
        warnings.simplefilter(action='ignore', category=UserWarning)
        numext_percent['contains_number'] = numext_percent['textl'].str.contains(number_pattern, regex=True)
        numext_percent['contains_range'] = numext_percent['textl'].str.contains(range_pattern, regex=True)
        numext_percent['contains_numeric'] = numext_percent['contains_number'] | numext_percent['contains_range']
    
        nums_ext = numext_percent[(numext_percent['contains_numeric'])]
    nums_ext = nums_ext.reset_index()
    nums_ext['text'] = nums_ext['text'].str.replace('covid-19', '')
    nums_ext['text'] = nums_ext['text'].str.replace('covid19', '')
    nums_ext['text'] = nums_ext['text'].str.replace('covid 19', '')
    
    return nums_ext


def extract_numbers(text):
    """
    get numbers from string of text
    """
    return re.findall(r'[+]?(?:\d*\.*\d+)', text)


def replace_fractions(num_sents, frac_dict):
    '''
    Iterate through sentences, replace fraction unicode with decimal strings
    '''
    fraction_pattern = r'[\u2150-\u215E\u00BC-\u00BE]'
    for idx, sent in num_sents.items():
        if re.findall(fraction_pattern, sent):
            for key, value in frac_dict.items():
                num_sents[idx] = num_sents[idx].replace(key, value)
    return(num_sents)


def extract_target_numbers(content, id_columns, models, target, target_list):
    '''
    Perform numerical extraction on cleaned text content that may contain numeric
    '''
    cleaned_df = numextract_pre_process(content, target_list)
    frac_dict = build_fraction_dict(cleaned_df.reset_index().text)
    target_sentences = replace_fractions(cleaned_df.reset_index().text, frac_dict)
      
    # Run QA Model
    qa = models["QA"]
    #torch.set_num_threads(24)
    print(f'running QA Model for {target.capitalize()} Extraction')
    target_number_texts = []
    for text in tqdm(target_sentences):
        res = find_target_num(text, target, qa.model)
        target_number_texts.append(res)
    target_numbers = [extract_numbers(i) for i in target_number_texts]
    
    # Run Zero-Shot Model
    zero = models["Zeroshot"]
    print(f'running Zero-shot Model for {target.capitalize()} Extraction')
    target_movement = []
    for text in tqdm(target_sentences):
        res = find_target_dir(text, target, zero.model)
        target_movement.append(res)
    
    df = pd.DataFrame([list(target_sentences), target_number_texts, target_numbers, target_movement]).transpose()
    df.columns = ['text','extract','numerics', 'direction']
    df['sign'] = df['direction'].apply(lambda x: sign_from_dir(x, target))
    with warnings.catch_warnings():
        warnings.simplefilter("ignore", category=RuntimeWarning)
        df[f'{target.capitalize()}Extract'] = [np.mean(list(map(float, i))) for i in df['numerics']]
        df[f'{target.capitalize()}Extract'] = df['sign'] *df[f'{target.capitalize()}Extract']
        
    df[id_columns] = cleaned_df.reset_index()[id_columns]

    return df[id_columns + ['text', 'extract', 'direction', f'{target.capitalize()}Extract']]

def build_fraction_dict(num_sents):
    '''
    scan all sentences, extract fractions and build dictionary mapping each fraction to 
    decimal.(example: '¼': '.25')
    '''
    fraction_pattern = r'[\u2150-\u215E\u00BC-\u00BE]'
    all_fractions = [re.findall(fraction_pattern, sent) for sent in num_sents if re.findall(fraction_pattern, sent)]
    fraction_set = set([item for sublist in all_fractions for item in sublist])
    frac_dict = {key:str(unicodedata.numeric(key)).replace('0','') for key in fraction_set}
    
    return frac_dict


    
