% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/pseudoData.R
\name{pseudoData}
\alias{pseudoData}
\title{R6 class for connection to anonymized database.}
\description{
R6 class for connection to anonymized database.

R6 class for connection to anonymized database.
}
\section{Super class}{
\code{shintodb::NA}
}
\section{Methods}{
\subsection{Public methods}{
\itemize{
\item \href{#method-NA-new}{\code{pseudoData$new()}}
\item \href{#method-NA-encrypt}{\code{pseudoData$encrypt()}}
\item \href{#method-NA-decrypt}{\code{pseudoData$decrypt()}}
\item \href{#method-NA-replace_na_char}{\code{pseudoData$replace_na_char()}}
\item \href{#method-NA-get_person_brp}{\code{pseudoData$get_person_brp()}}
\item \href{#method-NA-add_naam_column}{\code{pseudoData$add_naam_column()}}
\item \href{#method-NA-get_person_kenmerk}{\code{pseudoData$get_person_kenmerk()}}
\item \href{#method-NA-anummer_from_bsn}{\code{pseudoData$anummer_from_bsn()}}
\item \href{#method-NA-set_relation}{\code{pseudoData$set_relation()}}
\item \href{#method-NA-set_parent_relation}{\code{pseudoData$set_parent_relation()}}
\item \href{#method-NA-set_kind_relation}{\code{pseudoData$set_kind_relation()}}
\item \href{#method-NA-get_huwelijk}{\code{pseudoData$get_huwelijk()}}
\item \href{#method-NA-get_kinderen}{\code{pseudoData$get_kinderen()}}
\item \href{#method-NA-get_family}{\code{pseudoData$get_family()}}
\item \href{#method-NA-label_burgerlijke_staat}{\code{pseudoData$label_burgerlijke_staat()}}
\item \href{#method-NA-get_all_bronnen}{\code{pseudoData$get_all_bronnen()}}
\item \href{#method-NA-get_person_bronnen}{\code{pseudoData$get_person_bronnen()}}
\item \href{#method-NA-get_suite}{\code{pseudoData$get_suite()}}
\item \href{#method-NA-get_openwave}{\code{pseudoData$get_openwave()}}
\item \href{#method-NA-get_carel}{\code{pseudoData$get_carel()}}
\item \href{#method-NA-get_allegro}{\code{pseudoData$get_allegro()}}
\item \href{#method-NA-get_menscentraal}{\code{pseudoData$get_menscentraal()}}
\item \href{#method-NA-get_menscentraal_zaaktypes}{\code{pseudoData$get_menscentraal_zaaktypes()}}
\item \href{#method-NA-get_bronnen_count}{\code{pseudoData$get_bronnen_count()}}
\item \href{#method-NA-get_verhuizingen}{\code{pseudoData$get_verhuizingen()}}
\item \href{#method-NA-get_adreswijzigingen_sinds}{\code{pseudoData$get_adreswijzigingen_sinds()}}
\item \href{#method-NA-get_geboortes_sinds}{\code{pseudoData$get_geboortes_sinds()}}
\item \href{#method-NA-get_huwelijken_sinds}{\code{pseudoData$get_huwelijken_sinds()}}
\item \href{#method-NA-get_scheidingen_sinds}{\code{pseudoData$get_scheidingen_sinds()}}
\item \href{#method-NA-get_overlijdens_sinds}{\code{pseudoData$get_overlijdens_sinds()}}
\item \href{#method-NA-rest_lookup}{\code{pseudoData$rest_lookup()}}
\item \href{#method-NA-table_depseudo2}{\code{pseudoData$table_depseudo2()}}
\item \href{#method-NA-find_pseudo_columns}{\code{pseudoData$find_pseudo_columns()}}
\item \href{#method-NA-make_adres_display}{\code{pseudoData$make_adres_display()}}
\item \href{#method-NA-get_family_depseudo}{\code{pseudoData$get_family_depseudo()}}
\item \href{#method-NA-save_user_search_history}{\code{pseudoData$save_user_search_history()}}
\item \href{#method-NA-get_user_search_history_today}{\code{pseudoData$get_user_search_history_today()}}
\item \href{#method-NA-get_pseudolog_files}{\code{pseudoData$get_pseudolog_files()}}
\item \href{#method-unknown-clone}{\code{pseudoData$clone()}}
}
}
\if{html}{\out{
<details><summary>Inherited methods</summary>
<ul>
<li><span class="pkg-link" data-pkg="shintodb" data-topic="NA" data-id="append_data"><a href='../../shintodb/html/NA.html#method-NA-append_data'><code>shintodb::NA$append_data()</code></a></span></li>
<li><span class="pkg-link" data-pkg="shintodb" data-topic="NA" data-id="close"><a href='../../shintodb/html/NA.html#method-NA-close'><code>shintodb::NA$close()</code></a></span></li>
<li><span class="pkg-link" data-pkg="shintodb" data-topic="NA" data-id="connect_to_database"><a href='../../shintodb/html/NA.html#method-NA-connect_to_database'><code>shintodb::NA$connect_to_database()</code></a></span></li>
<li><span class="pkg-link" data-pkg="shintodb" data-topic="NA" data-id="delete_rows_where"><a href='../../shintodb/html/NA.html#method-NA-delete_rows_where'><code>shintodb::NA$delete_rows_where()</code></a></span></li>
<li><span class="pkg-link" data-pkg="shintodb" data-topic="NA" data-id="execute_query"><a href='../../shintodb/html/NA.html#method-NA-execute_query'><code>shintodb::NA$execute_query()</code></a></span></li>
<li><span class="pkg-link" data-pkg="shintodb" data-topic="NA" data-id="filter"><a href='../../shintodb/html/NA.html#method-NA-filter'><code>shintodb::NA$filter()</code></a></span></li>
<li><span class="pkg-link" data-pkg="shintodb" data-topic="NA" data-id="get_query"><a href='../../shintodb/html/NA.html#method-NA-get_query'><code>shintodb::NA$get_query()</code></a></span></li>
<li><span class="pkg-link" data-pkg="shintodb" data-topic="NA" data-id="has_connection"><a href='../../shintodb/html/NA.html#method-NA-has_connection'><code>shintodb::NA$has_connection()</code></a></span></li>
<li><span class="pkg-link" data-pkg="shintodb" data-topic="NA" data-id="has_value"><a href='../../shintodb/html/NA.html#method-NA-has_value'><code>shintodb::NA$has_value()</code></a></span></li>
<li><span class="pkg-link" data-pkg="shintodb" data-topic="NA" data-id="have_column"><a href='../../shintodb/html/NA.html#method-NA-have_column'><code>shintodb::NA$have_column()</code></a></span></li>
<li><span class="pkg-link" data-pkg="shintodb" data-topic="NA" data-id="list_indexes"><a href='../../shintodb/html/NA.html#method-NA-list_indexes'><code>shintodb::NA$list_indexes()</code></a></span></li>
<li><span class="pkg-link" data-pkg="shintodb" data-topic="NA" data-id="list_tables"><a href='../../shintodb/html/NA.html#method-NA-list_tables'><code>shintodb::NA$list_tables()</code></a></span></li>
<li><span class="pkg-link" data-pkg="shintodb" data-topic="NA" data-id="make_column"><a href='../../shintodb/html/NA.html#method-NA-make_column'><code>shintodb::NA$make_column()</code></a></span></li>
<li><span class="pkg-link" data-pkg="shintodb" data-topic="NA" data-id="postgres_now"><a href='../../shintodb/html/NA.html#method-NA-postgres_now'><code>shintodb::NA$postgres_now()</code></a></span></li>
<li><span class="pkg-link" data-pkg="shintodb" data-topic="NA" data-id="postgres_now_in_local_time"><a href='../../shintodb/html/NA.html#method-NA-postgres_now_in_local_time'><code>shintodb::NA$postgres_now_in_local_time()</code></a></span></li>
<li><span class="pkg-link" data-pkg="shintodb" data-topic="NA" data-id="query"><a href='../../shintodb/html/NA.html#method-NA-query'><code>shintodb::NA$query()</code></a></span></li>
<li><span class="pkg-link" data-pkg="shintodb" data-topic="NA" data-id="read_table"><a href='../../shintodb/html/NA.html#method-NA-read_table'><code>shintodb::NA$read_table()</code></a></span></li>
<li><span class="pkg-link" data-pkg="shintodb" data-topic="NA" data-id="replace_value_where"><a href='../../shintodb/html/NA.html#method-NA-replace_value_where'><code>shintodb::NA$replace_value_where()</code></a></span></li>
<li><span class="pkg-link" data-pkg="shintodb" data-topic="NA" data-id="replace_value_where2"><a href='../../shintodb/html/NA.html#method-NA-replace_value_where2'><code>shintodb::NA$replace_value_where2()</code></a></span></li>
<li><span class="pkg-link" data-pkg="shintodb" data-topic="NA" data-id="table_columns"><a href='../../shintodb/html/NA.html#method-NA-table_columns'><code>shintodb::NA$table_columns()</code></a></span></li>
<li><span class="pkg-link" data-pkg="shintodb" data-topic="NA" data-id="table_info"><a href='../../shintodb/html/NA.html#method-NA-table_info'><code>shintodb::NA$table_info()</code></a></span></li>
<li><span class="pkg-link" data-pkg="shintodb" data-topic="NA" data-id="update_where"><a href='../../shintodb/html/NA.html#method-NA-update_where'><code>shintodb::NA$update_where()</code></a></span></li>
<li><span class="pkg-link" data-pkg="shintodb" data-topic="NA" data-id="write_table_overwrite"><a href='../../shintodb/html/NA.html#method-NA-write_table_overwrite'><code>shintodb::NA$write_table_overwrite()</code></a></span></li>
</ul>
</details>
}}
\if{html}{\out{<hr>}}
\if{html}{\out{<a id="method-NA-new"></a>}}
\if{latex}{\out{\hypertarget{method-NA-new}{}}}
\subsection{Method \code{new()}}{
Create a new pseudoData (IZM) object
\subsection{Usage}{
\if{html}{\out{<div class="r">}}\preformatted{pseudoData$new(
  config_file = NULL,
  schema = "",
  filename = NULL,
  pool = FALSE,
  secret = ""
)}\if{html}{\out{</div>}}
}

\subsection{Arguments}{
\if{html}{\out{<div class="arguments">}}
\describe{
\item{\code{config_file}}{Location of the config file with database passwords.}

\item{\code{schema}}{Schema of the DB where data is stored.}

\item{\code{filename}}{If SQLite (so far, the default), path to the SQLite database file.}

\item{\code{pool}}{If TRUE, uses pool to connect.}
}
\if{html}{\out{</div>}}
}
\subsection{Returns}{
A new pseudoData object
}
}
\if{html}{\out{<hr>}}
\if{html}{\out{<a id="method-NA-encrypt"></a>}}
\if{latex}{\out{\hypertarget{method-NA-encrypt}{}}}
\subsection{Method \code{encrypt()}}{
Symmetric encrypt, using the secret
\subsection{Usage}{
\if{html}{\out{<div class="r">}}\preformatted{pseudoData$encrypt(x)}\if{html}{\out{</div>}}
}

}
\if{html}{\out{<hr>}}
\if{html}{\out{<a id="method-NA-decrypt"></a>}}
\if{latex}{\out{\hypertarget{method-NA-decrypt}{}}}
\subsection{Method \code{decrypt()}}{
Symmetric decrypt, using the secret
\subsection{Usage}{
\if{html}{\out{<div class="r">}}\preformatted{pseudoData$decrypt(x)}\if{html}{\out{</div>}}
}

}
\if{html}{\out{<hr>}}
\if{html}{\out{<a id="method-NA-replace_na_char"></a>}}
\if{latex}{\out{\hypertarget{method-NA-replace_na_char}{}}}
\subsection{Method \code{replace_na_char()}}{
Replace all NAs in a dataframe with ""
\subsection{Usage}{
\if{html}{\out{<div class="r">}}\preformatted{pseudoData$replace_na_char(data)}\if{html}{\out{</div>}}
}

\subsection{Arguments}{
\if{html}{\out{<div class="arguments">}}
\describe{
\item{\code{data}}{A dataframe (/tibble)}
}
\if{html}{\out{</div>}}
}
}
\if{html}{\out{<hr>}}
\if{html}{\out{<a id="method-NA-get_person_brp"></a>}}
\if{latex}{\out{\hypertarget{method-NA-get_person_brp}{}}}
\subsection{Method \code{get_person_brp()}}{
Read personal data based on pseudo-id(s).
\subsection{Usage}{
\if{html}{\out{<div class="r">}}\preformatted{pseudoData$get_person_brp(
  pseudo_id = NULL,
  what = c("bsn", "anr", "adres"),
  adres = NULL
)}\if{html}{\out{</div>}}
}

\subsection{Arguments}{
\if{html}{\out{<div class="arguments">}}
\describe{
\item{\code{pseudo_id}}{Vector of pseudo-ids.}

\item{\code{what}}{The pseudo-id is either a (pseudo) BSN, A-nummer, or adres.}

\item{\code{adres}}{If what="adres", provide the address as a list with components vblpostcode, etc.}
}
\if{html}{\out{</div>}}
}
\subsection{Details}{
Reads pseudo data from bzsprsq00.
}

\subsection{Returns}{
A dataframe
}
}
\if{html}{\out{<hr>}}
\if{html}{\out{<a id="method-NA-add_naam_column"></a>}}
\if{latex}{\out{\hypertarget{method-NA-add_naam_column}{}}}
\subsection{Method \code{add_naam_column()}}{
Util to add 'naam' column to depseudonized form of $get_person_brp,
for example in $get_family --> callModule(depseudo...)
\subsection{Usage}{
\if{html}{\out{<div class="r">}}\preformatted{pseudoData$add_naam_column(data, fill_na = NULL)}\if{html}{\out{</div>}}
}

}
\if{html}{\out{<hr>}}
\if{html}{\out{<a id="method-NA-get_person_kenmerk"></a>}}
\if{latex}{\out{\hypertarget{method-NA-get_person_kenmerk}{}}}
\subsection{Method \code{get_person_kenmerk()}}{
Get columns from bzsprsq00 for a vector of BSNs
\subsection{Usage}{
\if{html}{\out{<div class="r">}}\preformatted{pseudoData$get_person_kenmerk(pseudo_id, columns)}\if{html}{\out{</div>}}
}

}
\if{html}{\out{<hr>}}
\if{html}{\out{<a id="method-NA-anummer_from_bsn"></a>}}
\if{latex}{\out{\hypertarget{method-NA-anummer_from_bsn}{}}}
\subsection{Method \code{anummer_from_bsn()}}{
Find (pseudo) A-nummer from a (pseudo) BSN
\subsection{Usage}{
\if{html}{\out{<div class="r">}}\preformatted{pseudoData$anummer_from_bsn(pseudo_id)}\if{html}{\out{</div>}}
}

\subsection{Arguments}{
\if{html}{\out{<div class="arguments">}}
\describe{
\item{\code{pseudo_id}}{Vector of pseudo-ids.}
}
\if{html}{\out{</div>}}
}
\subsection{Returns}{
A vector
}
}
\if{html}{\out{<hr>}}
\if{html}{\out{<a id="method-NA-set_relation"></a>}}
\if{latex}{\out{\hypertarget{method-NA-set_relation}{}}}
\subsection{Method \code{set_relation()}}{
Set relation column in a dataframe
\subsection{Usage}{
\if{html}{\out{<div class="r">}}\preformatted{pseudoData$set_relation(data, relation)}\if{html}{\out{</div>}}
}

\subsection{Arguments}{
\if{html}{\out{<div class="arguments">}}
\describe{
\item{\code{data}}{A dataframe}

\item{\code{relation}}{The relation to be set}
}
\if{html}{\out{</div>}}
}
\subsection{Returns}{
Dataframe
}
}
\if{html}{\out{<hr>}}
\if{html}{\out{<a id="method-NA-set_parent_relation"></a>}}
\if{latex}{\out{\hypertarget{method-NA-set_parent_relation}{}}}
\subsection{Method \code{set_parent_relation()}}{
Write parent relation to dataframe
\subsection{Usage}{
\if{html}{\out{<div class="r">}}\preformatted{pseudoData$set_parent_relation(data)}\if{html}{\out{</div>}}
}

\subsection{Arguments}{
\if{html}{\out{<div class="arguments">}}
\describe{
\item{\code{Dataframe}}{with at least 'geslacht' column}
}
\if{html}{\out{</div>}}
}
\subsection{Details}{
If Geslacht = "M", set relation = "vader", otherwise "moeder"
}

\subsection{Returns}{
Dataframe
}
}
\if{html}{\out{<hr>}}
\if{html}{\out{<a id="method-NA-set_kind_relation"></a>}}
\if{latex}{\out{\hypertarget{method-NA-set_kind_relation}{}}}
\subsection{Method \code{set_kind_relation()}}{
Write child relation to dataframe
\subsection{Usage}{
\if{html}{\out{<div class="r">}}\preformatted{pseudoData$set_kind_relation(data)}\if{html}{\out{</div>}}
}

\subsection{Arguments}{
\if{html}{\out{<div class="arguments">}}
\describe{
\item{\code{Dataframe}}{with at least 'geslacht' column}
}
\if{html}{\out{</div>}}
}
\subsection{Details}{
If Geslacht = "M", set relation = "zoon", otherwise "dochter"
}

\subsection{Returns}{
Dataframe
}
}
\if{html}{\out{<hr>}}
\if{html}{\out{<a id="method-NA-get_huwelijk"></a>}}
\if{latex}{\out{\hypertarget{method-NA-get_huwelijk}{}}}
\subsection{Method \code{get_huwelijk()}}{
Read huwelijk data
\subsection{Usage}{
\if{html}{\out{<div class="r">}}\preformatted{pseudoData$get_huwelijk(pseudo_id, what = "bsn")}\if{html}{\out{</div>}}
}

\subsection{Arguments}{
\if{html}{\out{<div class="arguments">}}
\describe{
\item{\code{pseudo_id}}{Vector of pseudo-ids (BSN)}
}
\if{html}{\out{</div>}}
}
\subsection{Details}{
Read from bzshuwq00 table, based on provided pseudo-ids.
}

\subsection{Returns}{
Dataframe
}
}
\if{html}{\out{<hr>}}
\if{html}{\out{<a id="method-NA-get_kinderen"></a>}}
\if{latex}{\out{\hypertarget{method-NA-get_kinderen}{}}}
\subsection{Method \code{get_kinderen()}}{
Read children data
\subsection{Usage}{
\if{html}{\out{<div class="r">}}\preformatted{pseudoData$get_kinderen(pseudo_id)}\if{html}{\out{</div>}}
}

\subsection{Arguments}{
\if{html}{\out{<div class="arguments">}}
\describe{
\item{\code{pseudo_id}}{Vector of pseudo-ids (BSN)}
}
\if{html}{\out{</div>}}
}
\subsection{Details}{
Read from bzskinq00 table, based on provided pseudo-ids.
}

\subsection{Returns}{
Dataframe
}
}
\if{html}{\out{<hr>}}
\if{html}{\out{<a id="method-NA-get_family"></a>}}
\if{latex}{\out{\hypertarget{method-NA-get_family}{}}}
\subsection{Method \code{get_family()}}{
Read (pseudo) family data for provided pseudo-ids.
\subsection{Usage}{
\if{html}{\out{<div class="r">}}\preformatted{pseudoData$get_family(pseudo_id, what = c("bsn", "anr"))}\if{html}{\out{</div>}}
}

\subsection{Arguments}{
\if{html}{\out{<div class="arguments">}}
\describe{
\item{\code{pseudo_id}}{A single pseudo-id}
}
\if{html}{\out{</div>}}
}
\subsection{Details}{
Read BRP data for provided pseudo-id,
}

\subsection{Returns}{
Dataframe
}
}
\if{html}{\out{<hr>}}
\if{html}{\out{<a id="method-NA-label_burgerlijke_staat"></a>}}
\if{latex}{\out{\hypertarget{method-NA-label_burgerlijke_staat}{}}}
\subsection{Method \code{label_burgerlijke_staat()}}{
\subsection{Usage}{
\if{html}{\out{<div class="r">}}\preformatted{pseudoData$label_burgerlijke_staat(code)}\if{html}{\out{</div>}}
}

}
\if{html}{\out{<hr>}}
\if{html}{\out{<a id="method-NA-get_all_bronnen"></a>}}
\if{latex}{\out{\hypertarget{method-NA-get_all_bronnen}{}}}
\subsection{Method \code{get_all_bronnen()}}{
Retrieve all 'bronnen' for a person based on pseudo-id
\subsection{Usage}{
\if{html}{\out{<div class="r">}}\preformatted{pseudoData$get_all_bronnen(pseudo_bsn)}\if{html}{\out{</div>}}
}

\subsection{Arguments}{
\if{html}{\out{<div class="arguments">}}
\describe{
\item{\code{pseudo_bsn}}{A single pseudo-id (BSN) (not vectorized!)}
}
\if{html}{\out{</div>}}
}
\subsection{Returns}{
A reactive dataframe
}
}
\if{html}{\out{<hr>}}
\if{html}{\out{<a id="method-NA-get_person_bronnen"></a>}}
\if{latex}{\out{\hypertarget{method-NA-get_person_bronnen}{}}}
\subsection{Method \code{get_person_bronnen()}}{
Retrieve only depseudo bronnen (no BRP) for multiple persons
\subsection{Usage}{
\if{html}{\out{<div class="r">}}\preformatted{pseudoData$get_person_bronnen(pseudo_bsn)}\if{html}{\out{</div>}}
}

\subsection{Arguments}{
\if{html}{\out{<div class="arguments">}}
\describe{
\item{\code{pseudo_bsn}}{Vector of pseudo-ids, *not* reactive}
}
\if{html}{\out{</div>}}
}
}
\if{html}{\out{<hr>}}
\if{html}{\out{<a id="method-NA-get_suite"></a>}}
\if{latex}{\out{\hypertarget{method-NA-get_suite}{}}}
\subsection{Method \code{get_suite()}}{
\subsection{Usage}{
\if{html}{\out{<div class="r">}}\preformatted{pseudoData$get_suite(pseudo_bsn)}\if{html}{\out{</div>}}
}

}
\if{html}{\out{<hr>}}
\if{html}{\out{<a id="method-NA-get_openwave"></a>}}
\if{latex}{\out{\hypertarget{method-NA-get_openwave}{}}}
\subsection{Method \code{get_openwave()}}{
\subsection{Usage}{
\if{html}{\out{<div class="r">}}\preformatted{pseudoData$get_openwave(pseudo_id, what = "bsn_nummer")}\if{html}{\out{</div>}}
}

}
\if{html}{\out{<hr>}}
\if{html}{\out{<a id="method-NA-get_carel"></a>}}
\if{latex}{\out{\hypertarget{method-NA-get_carel}{}}}
\subsection{Method \code{get_carel()}}{
\subsection{Usage}{
\if{html}{\out{<div class="r">}}\preformatted{pseudoData$get_carel(pseudo_id)}\if{html}{\out{</div>}}
}

}
\if{html}{\out{<hr>}}
\if{html}{\out{<a id="method-NA-get_allegro"></a>}}
\if{latex}{\out{\hypertarget{method-NA-get_allegro}{}}}
\subsection{Method \code{get_allegro()}}{
\subsection{Usage}{
\if{html}{\out{<div class="r">}}\preformatted{pseudoData$get_allegro(pseudo_id)}\if{html}{\out{</div>}}
}

}
\if{html}{\out{<hr>}}
\if{html}{\out{<a id="method-NA-get_menscentraal"></a>}}
\if{latex}{\out{\hypertarget{method-NA-get_menscentraal}{}}}
\subsection{Method \code{get_menscentraal()}}{
\subsection{Usage}{
\if{html}{\out{<div class="r">}}\preformatted{pseudoData$get_menscentraal(pseudo_id)}\if{html}{\out{</div>}}
}

}
\if{html}{\out{<hr>}}
\if{html}{\out{<a id="method-NA-get_menscentraal_zaaktypes"></a>}}
\if{latex}{\out{\hypertarget{method-NA-get_menscentraal_zaaktypes}{}}}
\subsection{Method \code{get_menscentraal_zaaktypes()}}{
\subsection{Usage}{
\if{html}{\out{<div class="r">}}\preformatted{pseudoData$get_menscentraal_zaaktypes()}\if{html}{\out{</div>}}
}

}
\if{html}{\out{<hr>}}
\if{html}{\out{<a id="method-NA-get_bronnen_count"></a>}}
\if{latex}{\out{\hypertarget{method-NA-get_bronnen_count}{}}}
\subsection{Method \code{get_bronnen_count()}}{
\subsection{Usage}{
\if{html}{\out{<div class="r">}}\preformatted{pseudoData$get_bronnen_count(pseudo_id)}\if{html}{\out{</div>}}
}

}
\if{html}{\out{<hr>}}
\if{html}{\out{<a id="method-NA-get_verhuizingen"></a>}}
\if{latex}{\out{\hypertarget{method-NA-get_verhuizingen}{}}}
\subsection{Method \code{get_verhuizingen()}}{
\subsection{Usage}{
\if{html}{\out{<div class="r">}}\preformatted{pseudoData$get_verhuizingen(pseudo_id)}\if{html}{\out{</div>}}
}

}
\if{html}{\out{<hr>}}
\if{html}{\out{<a id="method-NA-get_adreswijzigingen_sinds"></a>}}
\if{latex}{\out{\hypertarget{method-NA-get_adreswijzigingen_sinds}{}}}
\subsection{Method \code{get_adreswijzigingen_sinds()}}{
\subsection{Usage}{
\if{html}{\out{<div class="r">}}\preformatted{pseudoData$get_adreswijzigingen_sinds(datum, pseudo_id)}\if{html}{\out{</div>}}
}

}
\if{html}{\out{<hr>}}
\if{html}{\out{<a id="method-NA-get_geboortes_sinds"></a>}}
\if{latex}{\out{\hypertarget{method-NA-get_geboortes_sinds}{}}}
\subsection{Method \code{get_geboortes_sinds()}}{
\subsection{Usage}{
\if{html}{\out{<div class="r">}}\preformatted{pseudoData$get_geboortes_sinds(datum, ouder_anr)}\if{html}{\out{</div>}}
}

}
\if{html}{\out{<hr>}}
\if{html}{\out{<a id="method-NA-get_huwelijken_sinds"></a>}}
\if{latex}{\out{\hypertarget{method-NA-get_huwelijken_sinds}{}}}
\subsection{Method \code{get_huwelijken_sinds()}}{
\subsection{Usage}{
\if{html}{\out{<div class="r">}}\preformatted{pseudoData$get_huwelijken_sinds(datum, bsn)}\if{html}{\out{</div>}}
}

}
\if{html}{\out{<hr>}}
\if{html}{\out{<a id="method-NA-get_scheidingen_sinds"></a>}}
\if{latex}{\out{\hypertarget{method-NA-get_scheidingen_sinds}{}}}
\subsection{Method \code{get_scheidingen_sinds()}}{
\subsection{Usage}{
\if{html}{\out{<div class="r">}}\preformatted{pseudoData$get_scheidingen_sinds(datum, bsn)}\if{html}{\out{</div>}}
}

}
\if{html}{\out{<hr>}}
\if{html}{\out{<a id="method-NA-get_overlijdens_sinds"></a>}}
\if{latex}{\out{\hypertarget{method-NA-get_overlijdens_sinds}{}}}
\subsection{Method \code{get_overlijdens_sinds()}}{
\subsection{Usage}{
\if{html}{\out{<div class="r">}}\preformatted{pseudoData$get_overlijdens_sinds(datum, bsn)}\if{html}{\out{</div>}}
}

}
\if{html}{\out{<hr>}}
\if{html}{\out{<a id="method-NA-rest_lookup"></a>}}
\if{latex}{\out{\hypertarget{method-NA-rest_lookup}{}}}
\subsection{Method \code{rest_lookup()}}{
Only perform the lookup method. Returns a reactive.
\subsection{Usage}{
\if{html}{\out{<div class="r">}}\preformatted{pseudoData$rest_lookup(pseudo_ids)}\if{html}{\out{</div>}}
}

}
\if{html}{\out{<hr>}}
\if{html}{\out{<a id="method-NA-table_depseudo2"></a>}}
\if{latex}{\out{\hypertarget{method-NA-table_depseudo2}{}}}
\subsection{Method \code{table_depseudo2()}}{
Depseudonimiseer een hele tabel
\subsection{Usage}{
\if{html}{\out{<div class="r">}}\preformatted{pseudoData$table_depseudo2(table_data = reactive(NULL), columns = NULL)}\if{html}{\out{</div>}}
}

\subsection{Details}{
Gepseudonimiseerde kolommen worden automatisch bepaald.
}

}
\if{html}{\out{<hr>}}
\if{html}{\out{<a id="method-NA-find_pseudo_columns"></a>}}
\if{latex}{\out{\hypertarget{method-NA-find_pseudo_columns}{}}}
\subsection{Method \code{find_pseudo_columns()}}{
\subsection{Usage}{
\if{html}{\out{<div class="r">}}\preformatted{pseudoData$find_pseudo_columns(data, length_pseudo = 9)}\if{html}{\out{</div>}}
}

}
\if{html}{\out{<hr>}}
\if{html}{\out{<a id="method-NA-make_adres_display"></a>}}
\if{latex}{\out{\hypertarget{method-NA-make_adres_display}{}}}
\subsection{Method \code{make_adres_display()}}{
\subsection{Usage}{
\if{html}{\out{<div class="r">}}\preformatted{pseudoData$make_adres_display(
  straatnaam,
  huisnummer,
  huisletter,
  huisnummertoevoeging,
  postcode,
  woonplaatsnaam
)}\if{html}{\out{</div>}}
}

}
\if{html}{\out{<hr>}}
\if{html}{\out{<a id="method-NA-get_family_depseudo"></a>}}
\if{latex}{\out{\hypertarget{method-NA-get_family_depseudo}{}}}
\subsection{Method \code{get_family_depseudo()}}{
\subsection{Usage}{
\if{html}{\out{<div class="r">}}\preformatted{pseudoData$get_family_depseudo(id_in = reactive(NULL))}\if{html}{\out{</div>}}
}

}
\if{html}{\out{<hr>}}
\if{html}{\out{<a id="method-NA-save_user_search_history"></a>}}
\if{latex}{\out{\hypertarget{method-NA-save_user_search_history}{}}}
\subsection{Method \code{save_user_search_history()}}{
\subsection{Usage}{
\if{html}{\out{<div class="r">}}\preformatted{pseudoData$save_user_search_history(pseudo_bsn, userid)}\if{html}{\out{</div>}}
}

}
\if{html}{\out{<hr>}}
\if{html}{\out{<a id="method-NA-get_user_search_history_today"></a>}}
\if{latex}{\out{\hypertarget{method-NA-get_user_search_history_today}{}}}
\subsection{Method \code{get_user_search_history_today()}}{
\subsection{Usage}{
\if{html}{\out{<div class="r">}}\preformatted{pseudoData$get_user_search_history_today(userid)}\if{html}{\out{</div>}}
}

}
\if{html}{\out{<hr>}}
\if{html}{\out{<a id="method-NA-get_pseudolog_files"></a>}}
\if{latex}{\out{\hypertarget{method-NA-get_pseudolog_files}{}}}
\subsection{Method \code{get_pseudolog_files()}}{
Most recent entry in `pseudolog` for each file
\subsection{Usage}{
\if{html}{\out{<div class="r">}}\preformatted{pseudoData$get_pseudolog_files()}\if{html}{\out{</div>}}
}

}
\if{html}{\out{<hr>}}
\if{html}{\out{<a id="method-unknown-clone"></a>}}
\if{latex}{\out{\hypertarget{method-unknown-clone}{}}}
\subsection{Method \code{clone()}}{
The objects of this class are cloneable with this method.
\subsection{Usage}{
\if{html}{\out{<div class="r">}}\preformatted{pseudoData$clone(deep = FALSE)}\if{html}{\out{</div>}}
}

\subsection{Arguments}{
\if{html}{\out{<div class="arguments">}}
\describe{
\item{\code{deep}}{Whether to make a deep clone.}
}
\if{html}{\out{</div>}}
}
}
}
