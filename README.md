# training-collection-app

Web application to browse bioinformatics training material on GitHub.

The code should work 'of the shelf'. The only thing you need to add to the repo base directory is a file called `.env.R` with the following contents:

```R
GITHUB_PAT="<an appropriate github personal access token>"
GITHUB_USER="<your github username>"
```

Find out more about generating a PAT [here](https://docs.github.com/en/authentication/keeping-your-account-and-data-secure/creating-a-personal-access-token). You don't need to select any scopes for this PAT. 
