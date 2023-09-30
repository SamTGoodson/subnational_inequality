import pandas as pd
import numpy as np

import re
import os 

import pycountry


save_folder_relative = os.path.join('..', '..', 'data', 'raw','national_election','eu_ned_national_nuts2(1).csv')
electoral_data_path = os.path.abspath(save_folder_relative)

ineq_folder_relative = os.path.join('..', '..', 'data', 'raw','lissy','multination_gini.txt')
inequality_data_path = os.path.abspath(ineq_folder_relative)

df_elections = pd.read_csv(electoral_data_path, delimiter=",", encoding='UTF-8')

df_inequality = pd.read_fwf(inequality_data_path, skiprows=3, header=None,
                                names=["file","region", "year", "avg_gini"])

def get_country_name(abbreviation):
    try:
        country = pycountry.countries.get(alpha_2=abbreviation)
        return country.name
    except AttributeError:
        return None


def clean_and_prepare(df_inequlaity,df_elections):
    df_inequality['country'] = df_inequality['file'].str[:2]
    df_inequality['country_name'] = df_inequality['country'].apply(get_country_name)
    df_inequality.dropna(subset=['region'], inplace=True)
    df_elections.dropna(subset=['regionname'], inplace=True)

    df_inequality['cleaned_region'] = df_inequality['region'].str.lower().apply(lambda x: re.sub(r'^.*-', '', x))
    df_elections['cleaned_region'] = df_elections['regionname'].str.lower().str.replace('[^\w\s]', '')

    df_inequality['cleaned_region'] = df_inequality['cleaned_region'].str.strip()

    countries_to_keep = df_inequality['country_name'].unique()
    df_elections = df_elections[df_elections['country'].isin(countries_to_keep)]

    return df_inequality, df_elections

def return_election_data(df_elections):
    elec_data = []

    for country in df_elections['country'].unique():
        country_df = df_elections[df_elections['country'] == country]
        years = country_df['year'].unique()
        for year in years:
            elec_data.append({'country': country, 'year': year})

    election_df = pd.DataFrame(elec_data)
    election_df['flag'] = 1

    return election_df

def make_join_df(df_inequality, election_df):

    inequality_with_elections = df_inequality.merge(election_df, left_on=['country_name', 'year'], right_on=['country', 'year'], how='outer')
    inequality_with_elections['flag'].fillna(0, inplace=True)

    df = inequality_with_elections[['country_name', 'cleaned_region', 'year', 'avg_gini', 'flag','country_y']]
    df['country_name'].fillna(df['country_y'], inplace=True)

    df.sort_values(['country_name', 'cleaned_region', 'year'], inplace=True)
    df['gini_since'] = df.groupby(['country_name', 'cleaned_region', 'flag'])['avg_gini'].transform(lambda x: x.expanding().mean().shift(fill_value=0))
    df['change_gini'] = df.groupby(['country_name', 'cleaned_region', 'flag'])['avg_gini'].transform(lambda x: x.diff().cumsum().shift(fill_value=0))

    election_years_df = df[df['flag'] == 1]
    election_years_df.reset_index(drop=True, inplace=True)
    
    joined_data = election_years_df.merge(df_elections, left_on=['country_name', 'cleaned_region', 'year'], right_on=['country', 'cleaned_region', 'year'], how='left')

    return joined_data