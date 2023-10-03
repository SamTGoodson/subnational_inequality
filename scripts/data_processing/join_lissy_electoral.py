import pandas as pd
import numpy as np

import re
import os 
import argparse

import pycountry


electoral_folder_relative = os.path.join('..', '..', 'data', 'raw','national_election','eu_ned_national_nuts2(1).csv')
electoral_data_path = os.path.abspath(electoral_folder_relative)

ineq_folder_relative = os.path.join('..', '..', 'data', 'raw','lissy','multination_gini.txt')
inequality_data_path = os.path.abspath(ineq_folder_relative)

results_folder_relative = os.path.join('..', '..', 'data', 'cleaned','national','joined_electoral_lissy.csv')
results_data_path = os.path.abspath(results_folder_relative)

df_elections = pd.read_csv(electoral_data_path, delimiter=",", encoding='UTF-8')

df_inequality = pd.read_fwf(inequality_data_path, skiprows=3, header=None,
                                names=["file","region", "year", "avg_gini"])

def clean_inequality_region(df):
    df.reset_index(drop=True, inplace=True)
    
    df.loc[df['country_name'] == 'Spain', 'cleaned_region'] = df['region'].str.lower().apply(lambda x: re.sub(r'\[\d+\](.+)', r'\1', x))
    df.loc[df['country_name'] != 'Spain', 'cleaned_region'] = df['region'].str.lower().apply(lambda x: re.sub(r'^.*-', '', x))
    df['cleaned_region'] = df['cleaned_region'].str.strip()

    return df


def get_country_name(abbreviation):
    try:
        country = pycountry.countries.get(alpha_2=abbreviation)
        return country.name
    except AttributeError:
        return None


def clean_and_prepare(df_inequality,df_elections):
    df_inequality['country'] = df_inequality['file'].str[:2]
    df_inequality['country_name'] = df_inequality['country'].apply(get_country_name)

    df_inequality.dropna(subset=['region'], inplace=True)
    df_elections.dropna(subset=['regionname'], inplace=True)

    df_inequality = clean_inequality_region(df_inequality)
    df_elections['cleaned_region'] = df_elections['regionname'].str.lower().str.replace('[^\w\s]', '')


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

    df = inequality_with_elections[['country_name', 'cleaned_region', 'year', 'avg_gini', 'flag', 'country_y']]
    
    
    df.loc[df['country_name'].isna(), 'country_name'] = df['country_y']
    
    df.sort_values(['country_name', 'cleaned_region', 'year'], inplace=True)
    
    
    df.loc[:, 'gini_since'] = df.groupby(['country_name', 'cleaned_region', 'flag'])['avg_gini'].transform(lambda x: x.expanding().mean().shift(fill_value=0))
    
    
    df.loc[:, 'change_gini'] = df.groupby(['country_name', 'cleaned_region', 'flag'])['avg_gini'].transform(lambda x: x.diff().cumsum().shift(fill_value=0))

    election_years_df = df[df['flag'] == 1].copy()
    election_years_df.reset_index(drop=True, inplace=True)

    joined_data = election_years_df.merge(df_elections, left_on=['country_name', 'cleaned_region', 'year'], right_on=['country', 'cleaned_region', 'year'], how='left')

    return joined_data


if __name__ == '__main__':
    parser = argparse.ArgumentParser()
    parser.add_argument('--results', type=str, default=results_data_path, help='Specify the output file path')
    args = parser.parse_args()

    try:
        df_inequality, df_elections = clean_and_prepare(df_inequality, df_elections)
        election_df = return_election_data(df_elections)
        joined_data = make_join_df(df_inequality, election_df)

        joined_data.to_csv(args.results, index=False)
        print("Script executed successfully. Data saved to:", args.results)
    except Exception as e:
        print("An error occurred:", str(e))