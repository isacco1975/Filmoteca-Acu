# 🎬 Sistema de Cadastro de Vídeos (Filmoteca Educ.360)

![language](https://img.shields.io/badge/language-COBOL-blue)
![status](https://img.shields.io/badge/status-Educacional-orange)
![license](https://img.shields.io/badge/license-MIT-green)

Este projeto é um sistema CRUD — acrônimo em inglês para **Create, Read, Update** e **Delete** — desenvolvido em **COBOL de plataforma baixa**, com foco na gestão de um acervo de vídeos.

Ele **não representa um produto final**, sendo um **requisito parcial para o Bootcamp de COBOL** promovido pela [Company.educ360](https://companye.academy/).

O sistema permite as operações de **inclusão, alteração, exclusão, listagem** e **importação/exportação de dados** em formato CSV (*comma-separated values*). O arquivo de dados utilizado é do tipo **indexado**, e a estrutura do código segue uma abordagem **modular**, com separação clara entre os programas principais, copybooks e arquivos de dados.

## 🎯 Objetivo

Trata-se de um sistema de controle pessoal de filmes assistidos, voltado para cinéfilos que desejam registrar informações detalhadas sobre cada título visto. O sistema permite armazenar dados como código, título, gênero, duração, distribuidora e uma nota atribuída ao filme, funcionando como um diário estruturado de sessões.

Além da finalidade de documentação, a funcionalidade de exportação permite que os dados sejam utilizados por ferramentas externas — como sistemas de recomendação baseados em inteligência artificial — para sugerir novos títulos, com base nos gêneros favoritos e nas notas mais altas atribuídas pelo usuário.

---

## 📁 Estrutura do Projeto

```
├── SRC/                       # Programas fonte COBOL, módulos principais
│   ├── CPY/                   # COPYBOOKS utilizados nos programas
├── DAT/                       # Arquivos de dados
│   ├── GENRE.dat              # Arquivo sequencial de dados de gêneros
│   ├── MOVIES.dat             # Arquivo indexado de dados de filmes
│   ├── MOVIES-IMP.csv         # Arquivos de importação (gerado automaticamente por ferramenta de IA)
│   ├── MOVIES-EXP.csv         # Arquivos de exportação
├── DOC/                       # Arquivos de documentação
│   ├── DBMODEL.png            # Definição lógica dos dados
│   ├── VIDEOTECA.png          # Definição dos arquivos de dados
│   ├── VIDEOTECA.drawio       # Diagrama dos componentes do sistema
│   ├── MNVIDPRG.png           # Diagrama dos componentes do menu principal
```

---

## 🧾 Estrutura do Arquivo de Dados

> Arquivo: `MOVIES.dat`
> Tipo: Indexado
> LRECL: 100

| Início | Término | Tamanho | Decimais | Tipo        | Label    | Descrição                                |
|:------:|:-------:|:-------:|:--------:|-------------|----------|------------------------------------------|
| 001    | 005     | 005     | 00       | Numérico    | CODFILME | Código de identificação única do filme   |
| 006    | 035     | 030     | 00       | Alfanumérico| TITULO   | Título do filme                          |
| 036    | 043     | 002     | 00       | Alfanumérico| GENERO   | Gênero de classificação do filme         |
| 044    | 046     | 003     | 00       | Numérico    | DURACAO  | Duração (contado em minutos)             |
| 047    | 061     | 015     | 00       | Alfanumérico| DISTRIB  | Nome da distribuidora                    |
| 062    | 063     | 002     | 00       | Numérico    | NOTA     | Nota atribuída ao filme                  |
| 064    | 100     | 037     | -        | Alfanumérico| FILLER   | Sem uso                                  |

> Arquivo: `GENRES.dat`
> Tipo: Sequencial
> LRECL: 15

| Início | Término | Tamanho | Decimais | Tipo        | Label    | Descrição                                |
|:------:|:-------:|:-------:|:--------:|-------------|----------|------------------------------------------|
| 001    | 003     | 002     | 00       | Numérico    | CODIGO   | Código de identificação única do gênero  |
| 003    | 011     | 008     | 00       | Alfanumérico| GENERO   | Nome do Gênero do filme                  |
| 012    | 015     | 004     | -        | Alfanumérico| FILLER   | Sem uso                                  |

> Modelo lógico dos dados

![1](https://github.com/fmarqueseti/Filmoteca-Educ360/blob/main/DOC/DBMODEL.png)

---

## 📌 Regras de Negócio

1. O sistema apresenta um menu de opções com a opção de saída ao final.
2. A inclusão deve validar duplicidade e solicitar confirmação antes de gravar:
   - Se confirmado: "Filme cadastrado com sucesso".
   - Senão, informar o motivo. (ex. filme já cadastrado)
3. A alteração exibe os dados atuais e solicita nova digitação:
   - Se confirmado: "Filme alterado com sucesso".
   - Senão, informar erro ao alterar.
4. A exclusão localiza e solicita confirmação:
   - Se confirmado: "Filme excluído com sucesso".
   - Senão, informar erro ao excluir.
5. A listagem mostra todos os registros em tela e retorna ao menu.
6. Permitir a importação dos dados a partir de um arquivo CSV (restauração de dados).
   - O arquivo deve existir na pasta `DAT/`.
   - Exibir a estatísticas de registros lidos, importados e ignorados.
   - Deve ser gerado um relatório com os registros ignorados (na pasta `DAT/`).
      - Se o arquivo existir, devem ser acrescentados os registros ignorados no final.
7. Permitir a exportação dos dados a um arquivo CSV (cópia de segurança dos dados).
   - Se o arquivo não existir, deve criá-lo na pasta `DAT/`.
   - Se existir, deve sobrescrevê-lo.
8. Permitir a exportação dos dados a um arquivo JSON.
   - Se o arquivo não existir, deve criá-lo na pasta `DAT/`.
   - Se existir, deve sobrescrevê-lo.
9. Adote boas práticas no desenvolvimento e modularização da solução, com foco em lógica estruturada e reusabilidade de código.

---

## 🧪 Pontos de Atenção (Quality Assurance)

- **Menu**:
  - UX mínima com navegação fácil.
  - Mensagem de erro para opção inválida.
- **Inclusão**:
  - Confirmação de gravação obrigatória.
- **Alteração**:
  - Confirmação obrigatória antes de sobrescrever.
- **Exclusão**:
  - Mensagem clara e confirmação antes de apagar.
- **Listagem**:
  - Paginação e exibição dos dados com opção de retorno ao menu.

> 💡 Em todas as operações do CRUD, o usuário deve ter:
> - Retorno visual claro das ações.
> - Capacidade de cancelar ou sair sem gravar.
> - Aceitação de "s" minúsculo ou "S" maiúsculo como confirmação.
> - Validação da nota (de zero a dez).
> - Validar o gênero informado em arquivo de dados, permitindo que o usuário liste todos os gêneros disponíveis. 

---

## 🧱 Padrões adotados

### Estrutura geral de nomes (8 caracteres): ```[XX][YYY][ZZZ]```
- ```XX```   = tipo ou módulo
- ```YYY```  = identificador do sistema (ex: VID, para "videoteca")
- ```ZZZ```  = função ou tipo específico

### Arquivos Fonte COBOL
| Tipo       | Nome     | Descrição                                        |
|------------|:--------:|--------------------------------------------------|
| Principal  | MNVIDPRG | Módulo principal do sistema (menu principal)     |
| Incluir    | INVIDPRG | Inclusão de registros                            |
| Alterar    | ALVIDPRG | Alteração de registros                           |
| Excluir    | EXVIDPRG | Exclusão de registros                            |
| Listar     | LSVIDPRG | Consulta em tela                                 |
| Restaurar  | IMVIDPRG | Importação de dados (CSV)                        |
| Backup     | XPVIDPRG | Exportação de dados (CSV)                        |
| Exportar   | JSVIDPRG | Exportação de dados (JSON)                       |
| Busca Gen  | SRVIDPRG | Módulo de busca, listagem e validação de gêneros |

### Arquivos de COPYBOOKS

#### Gerais (seção de armazenamento da área de trabalho)

| Tipo     | Nome     | Descrição                                                                  |
|----------|:--------:|----------------------------------------------------------------------------|
| Copybook | CPVIDABE | Variáveis da rotina de abend (*abnormal ending*)                           |
| Copybook | CPVIDDAT | Variáveis de definição do arquivo de acesso indexado de dados de filmes    |
| Copybook | CPVIDDTE | Variáveis da rotina de data/hora (usadas na tela principal)                |
| Copybook | CPVIDEDT | Variáveis da tela de edição                                                |
| Copybook | CPVIDFCV | Variáveis de descrição do arquivo de dados de filmes                       |
| Copybook | CPVIDFCW | Variáveis de controle (*status*) do arquivo de dados de filmes             |
| Copybook | CPVIDGEN | Variáveis de descrição do arquivo de dados de gêneros                      |
| Copybook | CPVIDGES | Variáveis de definição do arquivo de acesso sequencial de dados de gêneros |
| Copybook | CPVIDGEW | Variáveis de controle (*status*) do arquivo de dados de gêneros            |
| Copybook | CPVIDGSE | Rotinas de busca, listagem e validação de gêneros                          |
| Copybook | CPVIDMAN | Variáveis da tela principal                                                |
| Copybook | CPVIDMNU | Variáveis do menu principal                                                |
| Copybook | CPVIDMSG | Variáveis da rotina de mensagens                                           |
| Copybook | CPVIDRAB | Rotina de abend (*abnormal ending*)                                        |
| Copybook | CPVIDRVD | Rotina de validação de campos                                              |
| Copybook | CPVIDSEQ | Variáveis de definição do arquivo de acesso sequencial de dados de filmes  |
| Copybook | CPVIDSRC | Variáveis de tela da rotina de seleção de gênero                           |

#### Telas (screen)

| Tipo | Nome     | Descrição                                |
|------|:--------:|------------------------------------------|
| Tela | SCVIDBAN | Definição da tela do banner (ASCII ART)  |
| Tela | SCVIDEDT | Definição da tela de edição              |
| Tela | SCVIDGEN | Definição da tela de listagem de gêneros |
| Tela | SCVIDLST | Definição da tela da listagem em tela    |
| Tela | SCVIDMAN | Definição da tela principal              |
| Tela | SCVIDMNU | Definição da tela de menu                |
| Tela | SCVIDMSG | Definição da tela de mensagens           |

---
## 🗂️ Componentes do Sistema

![1](https://github.com/fmarqueseti/Filmoteca-Educ360/blob/main/DOC/MNVIDPRG.png)

---

## 🚀 Execução

A compilação e execução depende do ambiente. Exemplo do **GnuCOBOL**:

```
cobc -c INVIDPRG.cob
cobc -c ALVIDPRG.cob
cobc -c EXVIDPRG.cob
cobc -c LSVIDPRG.cob
cobc -c IMVIDPRG.cob
cobc -c XPVIDPRG.cob
cobc -c SRVIDPRG.cob
cobc -c JSVIDPRG.cob
cobc -x -o FILMOTECA MNVIDPRG.cob INVIDPRG.o ALVIDPRG.o EXVIDPRG.o LSVIDPRG.o IMVIDPRG.o XPVIDPRG.o SRVIDPRG.o JSVIDPRG.o
./FILMOTECA
```

> Certifique-se de que o diretório `DAT/` exista, que o arquivo `MOVIES.dat` possa ser criado, lido e modificado dentro dele, e que o arquivo `GENRE.dat` esteja presente.

---

## 🎥 Demonstração em Vídeo

Assista à apresentação do projeto no YouTube:
▶️ [https://youtu.be/Hq34DwSu0N4](https://youtu.be/Hq34DwSu0N4)

---

## 🤝 Contribuindo

Contribuições são bem-vindas! Sinta-se à vontade para abrir uma *issue*, propor uma *pull request* ou enviar sugestões.

Caso queira conversar diretamente, me procure pelo [LinkedIn](https://www.linkedin.com/in/fmrqs/).

---

## 📝 Licença

Este projeto é de uso educacional e está licenciado sob a licença [MIT](https://github.com/fmarqueseti/Filmoteca-Educ360?tab=MIT-1-ov-file).

---
