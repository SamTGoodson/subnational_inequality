import pandas as pd
import re
import os 
import argparse

from fuzzywuzzy import fuzz,process
from tqdm import tqdm
import pycountry
import warnings 
from unidecode import unidecode

electoral_folder_relative = os.path.join('..', '..', 'data', 'raw','national_election','eu_ned_national_nuts2(1).csv')
electoral_data_path = os.path.abspath(electoral_folder_relative)

ineq_folder_relative = os.path.join('..', '..', 'data', 'raw','lissy','multination_gini.txt')
inequality_data_path = os.path.abspath(ineq_folder_relative)

results_folder_relative = os.path.join('..', '..', 'data', 'cleaned','national','joined_electoral_lissy.csv')
results_data_path = os.path.abspath(results_folder_relative)

ineq_im_ed_folder_relative = os.path.join('..', '..', 'data', 'raw', 'lissy', 'multination_im_ed.txt')
ineq_im_ed_data_path = os.path.abspath(ineq_im_ed_folder_relative)

unemployment_data_path = os.path.abspath(os.path.join('..', '..', 'data', 'raw', 'lissy', 'unemployment.txt'))
df_unemployment = pd.read_fwf(unemployment_data_path, skiprows=3, header=None,
                              names=["file", "region", "year", "unemployment_rate"])

occupation_data_path = os.path.abspath(os.path.join('..', '..', 'data', 'raw', 'lissy', 'occupation.txt'))
df_occupation = pd.read_fwf(occupation_data_path, skiprows=3, header=None,
                            names=["file", "region", "year", "occupation_rate"])

df_elections = pd.read_csv(electoral_data_path, delimiter=",", encoding='UTF-8')

df_inequality = pd.read_fwf(inequality_data_path, skiprows=3, header=None,
                                names=["file","region", "year", "avg_gini"])

df_inequality_im_ed = pd.read_fwf(ineq_im_ed_data_path, skiprows=3, header=None,
                                    names=["file", "region", "year", "im_ratio", "ed_ratio"])

df_unemployment = pd.read_fwf(unemployment_data_path, skiprows=3, header=None,
                                    names=["file", "region", "year",'unemployment','immig'])
df_occupation = pd.read_fwf(occupation_data_path, skiprows=3, header=None,
                                    names=["file", "region", "year",'wage_ratio'])


manual_region_mapping = {
    # France
    'france': 'ile-de-france',
    'alpes': "provence-alpes-côte d’azur",
    "d'azur": "provence-alpes-côte d’azur",
    "pyrenees": "midi-pyrénées",
    "comte": "franche-comté",
    "roussillon": "languedoc-roussillon",
    "ardenne": "champagne-ardenne",
    # Spain
    'madrid': 'comunidad de madrid',
    'la mancha': 'castilla-la mancha',
    # Italy
    'friuli': 'friuli-venezia giulia',
    "valle d'aosta": "valle d'aosta/vallée d'aoste",
    'trentino': 'provincia autonoma di trento',
}

def clean_inequality_region(df):
    df.reset_index(drop=True, inplace=True)
    
    df.loc[df['country_name'] == 'Spain', 'cleaned_region'] = df['region'].str.lower().apply(lambda x: re.sub(r'\[\d+\](.+)', r'\1', x))
    df.loc[df['country_name'] != 'Spain', 'cleaned_region'] = df['region'].str.lower().apply(lambda x: re.sub(r'^.*-', '', x))
    df['cleaned_region'] = df['cleaned_region'].str.strip()

    return df

def clean_region(region):
    cleaned_region = re.sub(r'^\[\d+\](.+)', r'\1', region)
    cleaned_region = re.sub(r'^.*-', '', cleaned_region)
    cleaned_region = unidecode(cleaned_region)
    
    return cleaned_region.strip()

def get_country_name(abbreviation):
    try:
        country = pycountry.countries.get(alpha_2=abbreviation)
        return country.name
    except AttributeError:
        return None
    

def clean_im_ed_region(df):
    df.reset_index(drop=True, inplace=True)

    df['cleaned_region'] = df['region'].str.lower().apply(lambda x: re.sub(r'\[\d+\](.+)', r'\1', x))
    df['cleaned_region'] = df['cleaned_region'].str.strip()

    return df


def map_regions(df, df2):
    region_mapping = {}
    
    for region1 in tqdm(df['cleaned_region'], desc="Mapping regions"):
        match, score = find_best_match(region1, df2['cleaned_region'])

        if score >= 70:
            region_mapping[region1] = match
        else:
            region_mapping[region1] = region1  

    df['predicted_region'] = df['cleaned_region'].map(region_mapping)

    return df

def find_best_match(region, choices):
    match, score, index = process.extractOne(region, choices, scorer=fuzz.ratio)
    return match, score


def clean_and_prepare(df_inequality, df_elections, df_inequality_im_ed, df_unemployment, df_occupation):
    df_inequality['country'] = df_inequality['file'].str[:2]
    df_inequality['country_name'] = df_inequality['country'].apply(get_country_name)

    df_inequality_im_ed['country'] = df_inequality_im_ed['file'].str[:2]
    df_inequality_im_ed['country_name'] = df_inequality_im_ed['country'].apply(get_country_name)

    df_unemployment['country'] = df_unemployment['file'].str[:2]
    df_unemployment['country_name'] = df_unemployment['country'].apply(get_country_name)

    df_occupation['country'] = df_occupation['file'].str[:2]
    df_occupation['country_name'] = df_occupation['country'].apply(get_country_name)

    df_inequality.dropna(subset=['region'], inplace=True)
    df_elections.dropna(subset=['regionname'], inplace=True)
    df_inequality_im_ed.dropna(subset=['region'], inplace=True)
    df_unemployment.dropna(subset=['region'], inplace=True)
    df_occupation.dropna(subset=['region'], inplace=True)

    df_inequality['cleaned_region'] = df_inequality['region'].str.lower().apply(clean_region)
    df_elections['cleaned_region'] = df_elections['regionname'].str.lower().str.replace('[^\w\s]', '')
    df_inequality_im_ed['cleaned_region'] = df_inequality_im_ed['region'].str.lower().apply(clean_region)
    df_unemployment['cleaned_region'] = df_unemployment['region'].str.lower().apply(clean_region)
    df_occupation['cleaned_region'] = df_occupation['region'].str.lower().apply(clean_region)

    df_inequality = df_inequality.drop(['country','region'], axis=1)
    df_inequality_im_ed = df_inequality_im_ed.drop(['country','region'], axis=1)
    df_unemployment = df_unemployment.drop(['country','region'], axis=1)
    df_occupation = df_occupation.drop(['country','region'], axis=1)

    df_inequality = pd.merge(df_inequality, df_inequality_im_ed, on=["country_name","file", 
                                                                     "cleaned_region", "year"], how="left")
    df_inequality = pd.merge(df_inequality, df_unemployment, on=["country_name","file",
                                                                        "cleaned_region", "year"], how="left")
    df_inequality = pd.merge(df_inequality, df_occupation, on=["country_name","file",
                                                                        "cleaned_region", "year"], how="left")
    
    countries_to_drop = ['Belgium', 'Austria','Germany']
    df_inequality = df_inequality[~df_inequality['country_name'].isin(countries_to_drop)]

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

    regions = df_inequality[['country_name', 'cleaned_region']]
    regions.drop_duplicates(inplace=True)
    region_join = regions.merge(election_df, left_on=['country_name'], right_on=['country'], how='left')
    inequality_with_elections = df_inequality.merge(region_join, left_on=['country_name', 'cleaned_region', 'year'],
                                                    right_on=['country_name', 'cleaned_region', 'year'], how='outer')

    inequality_with_elections['flag'].fillna(0, inplace=True)

    df = inequality_with_elections[['country_name', 'cleaned_region', 'year', 'avg_gini', 'im_ratio', 'ed_ratio',
                                    'flag','unemployment','immig','wage_ratio']]    
    

    columns_to_interpolate = ['avg_gini', 'im_ratio', 'ed_ratio', 'unemployment', 'immig','wage_ratio']
    df_sorted = df.sort_values(by=['cleaned_region', 'year'])

    for column in columns_to_interpolate:
        df_sorted['interp_' + column] = df_sorted[column].transform(lambda x: x.interpolate())

    df = df_sorted
    
    df.sort_values(['country_name', 'cleaned_region', 'year'], inplace=True)
    df['delta_gini'] = df.groupby(['country_name', 'cleaned_region', 'flag'])['interp_avg_gini'].diff()
    df['avg_gini_period'] = df.groupby(['country_name', 'cleaned_region', 'flag'])['interp_avg_gini'].transform(lambda x: x.rolling(len(x), min_periods=1).mean())

    df['delta_im'] = df.groupby(['country_name', 'cleaned_region', 'flag'])['interp_im_ratio'].diff()
    df['avg_im_period'] = df.groupby(['country_name', 'cleaned_region', 'flag'])['interp_im_ratio'].transform(lambda x: x.rolling(len(x), min_periods=1).mean())

    df['delta_ed'] = df.groupby(['country_name', 'cleaned_region', 'flag'])['interp_ed_ratio'].diff()
    df['avg_ed_period'] = df.groupby(['country_name', 'cleaned_region', 'flag'])['interp_ed_ratio'].transform(lambda x: x.rolling(len(x), min_periods=1).mean())

    election_years_df = df[df['flag'] == 1].copy()
    election_years_df.reset_index(drop=True, inplace=True)

    election_years_df = map_regions(election_years_df, df_elections)
    election_years_df['predicted_region'] = election_years_df['predicted_region'].replace(manual_region_mapping)

    joined_data = election_years_df.merge(df_elections, left_on=['country_name', 'predicted_region', 'year'], right_on=['country', 'cleaned_region', 'year'], how='left')

    joined_data = joined_data[['country_name', 'cleaned_region_x', 'year', 'avg_gini', 'im_ratio', 'ed_ratio',
                            'delta_gini', 'avg_gini_period', 'delta_im', 'avg_im_period',
                            "interp_avg_gini", "interp_im_ratio", "interp_ed_ratio", "interp_unemployment", "interp_immig",
                            'interp_wage_ratio','delta_ed', 'avg_ed_period', 'party_abbreviation', 'party_english', 
                            'party_native', 'partyfacts_id',
                            'partyvote', 'electorate', 'totalvote', 'validvote']]
    
    joined_data.rename(columns={'cleaned_region_x': 'region'}, inplace=True)

    return joined_data


if __name__ == '__main__':
    parser = argparse.ArgumentParser()
    parser.add_argument('--results', type=str, default=results_data_path, help='Specify the output file path')
    args = parser.parse_args()

    try:
        warnings.filterwarnings("ignore")
        df_inequality, df_elections = clean_and_prepare(df_inequality, df_elections, df_inequality_im_ed, df_unemployment, df_occupation)
        election_df = return_election_data(df_elections)
        joined_data = make_join_df(df_inequality, election_df)

        joined_data.to_csv(args.results, index=False)
        print("Script executed successfully. Data saved to:", args.results)
    except Exception as e:
        print("An error occurred:", str(e))

