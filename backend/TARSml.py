"""ML code for enriching content."""

from abc import ABC, abstractmethod
from pathlib import Path
from transformers import AutoModelForQuestionAnswering, AutoTokenizer, pipeline
from tqdm import tqdm
import pandas as pd
import numpy as np
import pickle
from rbapy.core import pd_hstack
import re
import unicodedata
import spacy
from spacy.lang.en import English
import torch
import time
from importlib import resources


class TextEnrichmentModel(ABC):
    """A model that extracts info from text."""

    def __init__(self, name, prefix, device = 'cpu', text_column='text'):
        self.name = name
        self.prefix = prefix
        self.text_column = text_column
        self.device = device
        self.model = self._load_model()
        self._complete_init()

    @abstractmethod
    def _load_model(self):
        pass

    @abstractmethod
    def run_model(self, df):
        """
        Enrich the text column of the input dataframe.

        Returns a dataframe with the same number of rows (and row order) as the input
                and a column for each enrichment output.
        """
        print(f'Running model: {self.name} over {len(df)} rows.')

    @abstractmethod
    def creates_columns(self):
        """Return the names of the columns added by this model."""
        pass
    
    def validate_columns(self, df, id_columns):
        """
        Check that a dataframe contains all the expected columns for this model.
        
        Parameters
        ------------
        df: pd.DataFrame
            The dataframe to validate
        id_columns: list
            A list of the columns that combine to identify each row and are expected in the results
        
        """
        for c in id_columns:
            if c not in df.columns:
                raise ValueError(f'results missing required id_column: {c}')
        for c in self.creates_columns():
            if c not in df.columns:
                raise ValueError(f"results missing expected column: {c}")
        

    def _complete_init(self):
        """Run at the end of init, override to add custom initialisation."""
        pass

    def enrich(self, df, id_columns):
        """Return a dataframe with additional columns added by the model."""

        in_both = set(df.columns).intersection(self.creates_columns())
        if len(in_both) > 0:
            raise ValueError(f"Model {self.name} creates columns already in df: {in_both}")

        ids = df[list(id_columns)]
        scores = self.run_model(df)

        if set(scores.columns) != self.creates_columns():
            raise ValueError(f"Model {self.name} created columns:{scores.columns}, expected: {self.creates_columns}")
        if len(scores) != len(df):
            raise ValueError(f"Model {self.name} did not return one row per input row.")

        scores = pd_hstack([ids, scores])
        enriched = pd.merge(df, scores, how='inner',on=id_columns)
        if len(enriched) != len(df):
            raise ValueError(f"Model {self.name} produced {len(enriched)} rows. Expected: {len(df)}.")
        return enriched

    def _fix_column_name(self, name):
        """Remove special characters to make a valid SQL column name."""
        name = name.strip()
        fixed = re.sub(r'[^a-zA-Z0-9]','_',name)
        fixed = re.sub(r'_+','_',fixed)
        return fixed

# +
class IndustryModel(TextEnrichmentModel):
    """A model to predict what industry a paragraph of text may be referencing."""
    def _complete_init(self):
        self.labels =  [
            'agriculture',
            'construction',
            'defence or government',
            'education',
            'health',
            'manufacturing',
            'mining',
            'retail',
            'tourism',
            'transport',
            'utilities'
        ]
        self.top_labels = ['top_industry','top_industry_score']
        self.name_fix = {c:self._fix_column_name(self.prefix+c) for c in self.labels}
        self.columns_created = set(self.name_fix.values()).union(self.top_labels)
        self.multi_label = True

    def _load_model(self):
        device = self.device
        classifier = pipeline(
            "zero-shot-classification",
            model="facebook/bart-large-mnli",
            device = device
        )
        return classifier

    def creates_columns(self):
        return self.columns_created

    def run_model(self, df):
        super().run_model(df)
        results = []
        for i in tqdm(range(len(df))):
            row = df.iloc[i]
            text = row[self.text_column]
            result = self.model(text, self.labels, multi_label=self.multi_label)
            output_row = dict(zip(result['labels'],result['scores']))
            results.append(output_row)
        results = pd.DataFrame.from_records(results)
        results[self.top_labels[0]] = results[self.labels].idxmax(axis=1)
        results[self.top_labels[1]] = results[self.labels].max(axis=1)
        results.rename(columns=self.name_fix, inplace=True)
        return results


# -
class SentimentModel(TextEnrichmentModel):
    """Apply finbert to detect the sentiment of the text in the dataframe."""

    def _load_model(self):
        device = self.device
        sentiment = pipeline(
            'sentiment-analysis',
            model='ProsusAI/finbert',
            device = device
        )
        return sentiment

    def creates_columns(self):
        return set(['sentiment','sentiment_score'])

    def run_model(self, df):
        super().run_model(df)
        result_map = {'neutral':0,'negative':-1,'positive':1}
        results = []
        for i in tqdm(range(len(df))):
            row = df.iloc[i]
            text = row[self.text_column]
            result = self.model(text)[0]
            sentiment = result_map[result['label']]
            score = result['score']*sentiment
            output_row = {'sentiment_score':score, 'sentiment':sentiment}
            results.append(output_row)
        results = pd.DataFrame.from_records(results)
        return results



class CategoryModel(TextEnrichmentModel):
    """A model to predict what topic a paragraph of text may be referencing. Using a Zero-shot Classification model like BART-large-mnli"""
    
    def _complete_init(self):
        self.labels =  [
            'financing conditions',
            'investment or capex',
            'employment',
            'wages',
            'costs',
            'prices',
            'margins',
            'supply chains',
            'demand',
            'sales',
            'climate change',
            'property or housing',
            'non-labour costs',
            'labour costs'
        ]
        self.top_labels = ['top_category','top_score']
        self.name_fix = {c:self._fix_column_name(self.prefix+c) for c in self.labels}
        self.columns_created = set(self.name_fix.values()).union(self.top_labels)
        self.multi_label = True

    def _load_model(self):
        device = self.device
        classifier = pipeline(
            "zero-shot-classification",
            model="facebook/bart-large-mnli",
            device = device
        )
        return classifier

    def creates_columns(self):
        return self.columns_created

    def run_model(self, df):
        super().run_model(df)
        results = []
        for i in tqdm(range(len(df))):
            row = df.iloc[i]
            text = row[self.text_column]
            result = self.model(text, self.labels, multi_label=self.multi_label)
            output_row = dict(zip(result['labels'],result['scores']))
            results.append(output_row)
        results = pd.DataFrame.from_records(results)
        results[self.top_labels[0]] = results[self.labels].idxmax(axis=1)
        results[self.top_labels[1]] = results[self.labels].max(axis=1)
        results.rename(columns=self.name_fix, inplace=True)
        return results

    
class QA:
    """A question and answering model (roberta) to predict a numerical magnitude of percentage change in a paragraph of text."""
    def __init__(self, device = 'cpu'):
        self.name = 'deepset/roberta-base-squad2'
        self.model = pipeline('question-answering', model=self.name, tokenizer=self.name, device = device)


class Zero:
    """A model to predict in what direction a numerical percentage is changing in a paragraph of text. Using a Zero-shot Classification model like BART-large-mnli"""
    def __init__(self,  device = 'cpu'):
        self.name = 'facebook/bart-large-mnli'
        self.model = pipeline("zero-shot-classification",
                             model=self.name,
                             device = device)
