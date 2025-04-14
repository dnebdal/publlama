# Description
Publlama can run queries against pubmed using [rentrez](https://cran.r-project.org/web/packages/rentrez/index.html), 
and ask local LLMs questions about the abstracts of those articles through the [ollama](API).

# Installation
## Linux
When installing on linux, you may run into problems installing the dependencies.
This is because the packages that publlama depends on are themselves dependent
on tools or libraries that may not be installed on your system.

### Fedora
This should cover all the dependencies: 

`sudo dnf install @development-tools curl-devel libxml2-devel redhat-rpm-config R-core-devel ` 

### Ubuntu
`sudo apt update`

`sudo apt install build-essential libcurl4-openssl-dev libxml2-dev libssl-dev r-base-dev`

## Installation
Then try installing with either of the following methods:
### Manual:
```
git clone https://github.com/dnebdal/publlama
R CMD INSTALL publlama
```

### devtools/remotes:
Remotes: `remotes::install_github("dnebdal/publlama")`

Devtools: `devtools::install_github("dnebdal/publlama")`

# Entrez API key
To query pubmed at a reasonable speed, you need an API key. To get one, log in or create an account at
https://account.ncbi.nlm.nih.gov/ , and then when logged in, go to [your account/account settings](https://account.ncbi.nlm.nih.gov/settings), 
API Key Management, and create a key. 

# First run
The first time you run `publlamaInit()`, it creates a directory `publlama_db` in your current working directory,
and writes a default `settings.xml` and an empty `database.sqlite` there. 

You need to edit the settings.xml file to add your own queries and prompts, but there are placeholder examples.

This is also where you put your Entrez API key: Either directly in the settings file (with `<key>yourkey</key>`), 
or by saving the key to a text file and using <file>path/to/keyfile</file>. I recommend using a keyfile and 
storing it somewhere outside your project directory, to reduce the risk of accidentally publishing it when sharing your work.

The next time you run `publlamaInit()`, it will (re-) load your settings.xml file. 

# Configuration
## Prompts
You can write one or more prompts to ask your LLM model(s) in the settings file. There is an example, but in short:
- A prompt is a natural language question that will be sent to an LLM
- Include `%s` somewhere in your prompt. It will be replaced with the text of an abstract.
- While not strictly necessary, I recommend asking for the answer to be formatted as JSON;
 it makes it easier to extract and tabulate the answers when you run a large number of abstracts.
- Including a JSON template ("please format the answer as JSON and use this template") seems to increase the chance of
all the answers being formatted the same and thus being easier to tabulate
- Since the settings file is XML, the prompts can be written on multiple lines and include most characters, 
but `<` and `>` have to be written as `&lt;` and `&gt;`, respectively.
- As gathered from pubmed, the abstracts are in XML format. This doesn't seem to matter to the models I've tested, 
but they typically look something like this:
```xml
<AbstractText Label="INTRODUCTION" NlmCategory="UNASSIGNED"> In recent years, 
chronic lymphocytic leukemia (CLL) treatment has changed dramatically. 
Chemoimmunotherapy with fludarabine/cladribine, cyclophosphamide, and rituximab 
have been almost completely replaced by targeted therapies with small molecules, 
such as Bruton's tyrosine kinase inhibitors or B-cell lymphoma 2 (BCL-2) 
antagonists. However, few studies have assessed the impact of novel therapies on 
patient quality of life (QoL).</AbstractText>

<AbstractText Label="AREAS COVERED" NlmCategory="UNASSIGNED"> This article 
reviews the safety profile of new therapeutic options and their impact on the 
QoL of CLL patients. The MEDLINE database was searched for English language 
publications from 2010 through June 2024, including the Proceedings of the 
American Society of Hematology from over the past 5 years.</AbstractText>

<AbstractText Label="EXPERT OPINION" NlmCategory="UNASSIGNED"> CLL is a 
clinically heterogenous disease predominantly affecting elderly patients. The 
variable clinical course of disease requires personalization and individualized 
treatment to achieve the optimal survival outcome and acceptable safety profile, 
especially in the case of poor prognosis. Clinical trials performed in the past 
decade indicate that novel drugs, used as a single agent or as part of a 
conventional chemotherapy, offer promise in minimalizing relapse rates, and may 
allow more effective and safer treatment options by reducing the risk of adverse 
events, especially cytopenias and infections.</AbstractText>

```

An example prompt could be something like this:
```xml
<prompt name="isCancer">
Here is an abstract of a scientific article. Can you answer the following questions about it?
Q1: Is this article about drug treatment of a specific cancer?
Q2: If it is, which type of cancer? If not applicable, say "NA".
Q3: If it is, which drug or drugs? If not applicable, say "NA".

Please answer as JSON, using this template and with no explanations:
{ "Q1": your answer
  "Q2": ...
}

%s
</prompt>
```

## Queries
The queries are any valid search you could run on pubmed. The easiest way to construct a search is probably to
go to the [Advanced Search](https://pubmed.ncbi.nlm.nih.gov/advanced/) page. As you add terms, the text in the 
query box is a valid query. Feel free to modify it with brackets and AND/OR terms.  
Queries have a name and a category. Right now the category is not used for anyhthing, but has to be included.

Feel free to use multiple lines and indentation to make complicated queries more readable.

An example query can be something like this:
```xml
<query name="brca" category="drugs">
    (breast cancer[Title/Abstract])
AND (immunotherapy[Title/Abstract] OR chemotherapy[Title/Abstract])
AND ("2025/01/01"[Date - Create] : "3000"[Date - Create])
AND (epigenetic[Title/Abstract] OR epigenetics[Title/Abstract])
</query>
```

# Example
Assuming that you have already run `publlamaInit()` once, and added the above prompt and query to settings.xml,
and added your entrez API key, this will run the pubmed query, then get the articles published on or after the 
31st of January, and ask phi4:latest on a local ollama instance the above question about each article in turn:

```R
library(publlama)

settings = publlamaInit()
queries = getQueries()
lapply(queries, searchPubmed)

articles = getArticles(from="2025-01-31")
answers = askLLMVec("phi4:latest", "isCancer", articles, "localhost")
```

# TODO:
Lots.
