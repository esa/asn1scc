
Language server Protocol
 - high level overview : https://microsoft.github.io/language-server-protocol/overviews/lsp/overview/
 - official description of the protocol : https://microsoft.github.io/language-server-protocol/specifications/specification-3-14/

Existing Implementations
 - https://microsoft.github.io/language-server-protocol/implementors/servers/
 * I checked out the C# and F# implementation. ==> Huge implementations

There are seems to exist SDKs that makes the implementation easier

- https://microsoft.github.io/language-server-protocol/implementors/sdks/

Most promising one is the 
  https://github.com/OmniSharp/csharp-language-server-protocol
  donwloaded localy in C:\prj\GitHub\csharp-language-server-protocol
  There exists a sample  project in the Sample\SampleServer directory which I must study further.
  
Another SDK is this oneQ
	https://github.com/matarillo/LanguageServerProtocol
	which also provides an example
	https://github.com/matarillo/vscode-languageserver-csharp-example
	for both server and client for VS CODE
	
VSCODE api for extensions (required to spawn the language server) 
	https://code.visualstudio.com/api
	and
	https://code.visualstudio.com/api/language-extensions/language-server-extension-guide
	

sample from microsoft	

1. cd c:\prj\GitHub\
2. git clone https://github.com/Microsoft/vscode-extension-samples.git
3. cd vscode-extension-samples/lsp-sample
4. npm install
   (see https://docs.npmjs.com/cli/install)
   installs localy all modules listed as dependencies in package.json
   The modules are installed within the node_modules folder
   For a description of package.json se https://nodejs.org/en/knowledge/getting-started/npm/what-is-the-file-package-json/ (What is the file `package.json`?)
4. npm run compile
   npm run will open the package.json and execute the script with name compile under the section scripts
5. code .
   open vscode in current directory (vscode opens current directory)   
   
Explaining the 'Language Client'
Let's first take a look at /package.json, which describes the capabilities of the Language Client. 
There are three interesting sections:

First look the activationEvents:

A - Section activationEvents
"activationEvents": [
    "onLanguage:plaintext"
]
	--> activate on *.txt files 

B- The section main
	"main": "./client/out/extension"
	it indicates the enty point javascript file for this plugin (output of the extension.ts TypeScript file)
	
C - configuration section
"configuration": {
    "type": "object",
    "title": "Example configuration",
    "properties": {
        "languageServerExample.maxNumberOfProblems": {
            "scope": "resource",
            "type": "number",
            "default": 100,
            "description": "Controls the maximum number of problems produced by the server."
        }
    }
}
	
	
The actual Language Client code and the corresponding package.json is in the /client folder. 
The interesting part in the /client/package.json file is that it adds a dependency to the vscode extension host API and the vscode-languageclient library:	
	"engines": {
		"vscode": "^1.43.0"
	},
	"dependencies": {
		"vscode-languageclient": "^6.1.3"
	}

The source code of the client is within the extension.ts
 * it provides two functions activate() and deactivate()
 * The activate function call the .start method of the globally defined client: LanguageClient; object after 
 providing the correct initialization.
 
 

== Another example is here for the Dot (graphviz language) https://tomassetti.me/language-server-dot-visual-studio/
   and repo here https://github.com/gabriele-tomassetti/language-server-dot
 
