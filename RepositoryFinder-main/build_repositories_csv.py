#!/usr/bin/env python3
"""Regenerate data/repositories.csv and data/community_databases.csv.

Several text fields contain commas, so both files are written by a real CSV
writer. Edit this script and re-run it rather than hand-editing the CSVs:
one unquoted comma silently shifts every later column in a row.

Conventions in repositories.csv
  * access, license, license_terms, domain: semicolon-separated lists.
  * license = the actual licence names, shown on the cards.
  * license_terms = what the licence lets others do, used by the filter:
    No restrictions / Credit required / Share alike / Non-commercial only / Custom.
  * domain values must be GEMET Subitems (data/gemet_hierarchical_listing_*.csv).
  * domain_scope "any" = generalist: never excluded by the domain filter.
  * max_size_gb / embargo_max_months: a number, Inf for no limit, NA if unknown.
  * version_control / api_access / doi_service / affiliation_required: Yes, No or NA.
    affiliation_required is derived from eligibility: "Members only" -> Yes,
    "Open" or "Open with fees" (anyone may deposit, possibly paying) -> No.
  * NA never excludes a repository in the app.
  * confidence (High / Medium / Low) is kept for maintainers; the app does not
    show it, but uses it to put the best-documented repositories first.
"""
import csv
import os

HERE = os.path.dirname(os.path.abspath(__file__))
DATA = os.path.join(HERE, "data")

COLUMNS = [
    "name", "domain_scope", "domain", "access", "license", "license_terms", "max_size_gb",
    "embargo_supported", "embargo_max_months",
    "version_control", "api_access", "doi_service", "affiliation_required",
    "eligibility", "institutions",
    "metadata_req", "data_standard_req", "license_req",
    "confidence", "notes", "url",
]

ALL_CC = "CC0;CC BY;CC BY-SA;CC BY-NC;CC BY-NC-SA"
WUR = "Wageningen University & Research"
FOUR_TU = ";".join([
    "Delft University of Technology",
    "Eindhoven University of Technology",
    "University of Twente",
    WUR,
])

ROWS = [
 # ---------------------------------------------------------------- 1-14
 dict(name="Zenodo", domain_scope="any", domain="environment",
   access="Open;Restricted;Closed", license=ALL_CC, max_size_gb="200",
   embargo_supported="Yes", embargo_max_months="Inf",
   version_control="Yes", api_access="Yes", doi_service="Yes",
   eligibility="Open", institutions="NA",
   metadata_req="DataCite mandatory fields; no discipline-specific requirements.",
   data_standard_req="None imposed. Any file format accepted.",
   license_req="A licence must be chosen for every public file. All CC options are available.",
   confidence="High",
   notes="50 GB per record and 100 files by default; a one-time increase to 200 GB can be requested. The embargo date is set by the depositor with no upper limit. New versions share a concept DOI, so citations can point to one version or to all. REST API for deposit and retrieval. Free.",
   url="https://about.zenodo.org/policies"),

 dict(name="4TU.ResearchData", domain_scope="any", domain="environment",
   access="Open;Restricted;Closed", license=ALL_CC, max_size_gb="NA",
   embargo_supported="Yes", embargo_max_months="24",
   version_control="Yes", api_access="Yes", doi_service="Yes",
   eligibility="Open with fees", institutions=FOUR_TU,
   metadata_req="Rich descriptive metadata; a curator reviews every deposit.",
   data_standard_req="Open and exchangeable formats strongly encouraged; a preferred-format list is published.",
   license_req="CC0 is the default; other CC licences can be selected.",
   confidence="High",
   notes="Embargo capped at two years after deposit. WUR is a 4TU partner, so deposit is free for WUR staff. CoreTrustSeal certified with a 15-year preservation guarantee. No published size cap - contact them above roughly 1 TB.",
   url="https://data.4tu.nl/info/en/use/publish-cite/upload-your-data-in-our-data-repository"),

 dict(name="DANS Data Stations", domain_scope="any", domain="environment",
   access="Open;Restricted", license=ALL_CC, max_size_gb="NA",
   embargo_supported="Yes", embargo_max_months="24",
   version_control="Yes", api_access="Yes", doi_service="Yes",
   eligibility="Open", institutions="NA",
   metadata_req="Mandatory Dataverse metadata block, which differs per Data Station.",
   data_standard_req="National and international standards preferred.",
   license_req="CC0 is the default; the DANS licence applies to restricted files.",
   confidence="Medium",
   notes="Embargo is set per file. DANS encourages six months or less and recommends two years as a maximum, but does not technically enforce it. Restricted access requires a Data Access Protocol. Built on Dataverse, which versions every dataset and offers a full API.",
   url="https://dans.knaw.nl/en/depositing-data-manual/during-depositing_ds/"),

 dict(name="Dryad", domain_scope="any", domain="biosphere",
   access="Open", license="CC0", max_size_gb="300",
   embargo_supported="Yes", embargo_max_months="NA",
   version_control="Yes", api_access="Yes", doi_service="Yes",
   eligibility="Open with fees", institutions="NA",
   metadata_req="DataCite metadata plus a README; staff curate every dataset.",
   data_standard_req="Non-proprietary formats preferred; CSV, TSV or ODF for tables.",
   license_req="CC0 only. No other licence is accepted.",
   confidence="Medium",
   notes="300 GB per dataset. The standard route is Private for Peer Review rather than a timed embargo - Dryad usually does not grant embargoes but considers case-by-case requests with journal confirmation, so no maximum exists. Rejects personally identifiable data and flags vulnerable or endangered species records. Fee-based unless your institution holds a membership.",
   url="https://datadryad.org/stash/faq"),

 dict(name="PANGAEA", domain_scope="specific",
   domain="environment;hydrosphere;lithosphere;atmosphere;biosphere",
   access="Open;Restricted", license="CC BY;CC0;CC BY-NC", max_size_gb="NA",
   embargo_supported="Yes", embargo_max_months="24",
   version_control="Yes", api_access="Yes", doi_service="Yes",
   eligibility="Open", institutions="NA",
   metadata_req="Full editorial curation; parameters are mapped to PANGAEA vocabularies.",
   data_standard_req="Georeferenced observational and experimental data; harmonised on ingest.",
   license_req="CC BY recommended and CC0 possible; NC and ND are discouraged.",
   confidence="High",
   notes="Calls it a moratorium rather than an embargo - password protection for a maximum of two years after submission, with metadata always public. Published datasets cannot be changed; a correction is submitted as a new version and the old one is linked to it. Curation means publication can take eight weeks or more. Free to submit.",
   url="https://wiki.pangaea.de/wiki/Data_submission"),

 dict(name="GBIF", domain_scope="specific", domain="biosphere",
   access="Open", license="CC0;CC BY;CC BY-NC", max_size_gb="NA",
   embargo_supported="No", embargo_max_months="NA",
   version_control="Yes", api_access="Yes", doi_service="Yes",
   eligibility="Open", institutions="NA",
   metadata_req="Ecological Metadata Language (EML) via an IPT installation.",
   data_standard_req="Darwin Core Archive or Camtrap DP.",
   license_req="Only CC0, CC BY or CC BY-NC are permitted by governing-board decision.",
   confidence="High",
   notes="No embargo mechanism - you control timing by choosing when to register the dataset. Publishing runs through an IPT, which keeps numbered versions of each dataset, so GBIF complements an archive rather than replacing one. Sensitive species records can be published with generalised coordinates. Camera-trap data is normally annotated in Agouti and exported as Camtrap DP for publication here.",
   url="https://www.gbif.org/publishing-data"),

 dict(name="Movebank", domain_scope="specific", domain="biosphere",
   access="Open;Restricted", license="CC0", max_size_gb="NA",
   embargo_supported="NA", embargo_max_months="NA",
   version_control="NA", api_access="Yes", doi_service="Yes",
   eligibility="Open", institutions="NA",
   metadata_req="Movebank study metadata; Data Repository deposits also get DataCite metadata.",
   data_standard_req="Movebank attribute dictionary for tracking data.",
   license_req="Data Repository deposits are released under CC0.",
   confidence="Low",
   notes="Two services under one name. Studies in Movebank itself are private by default and can stay private indefinitely, which is closer to restricted access than a timed embargo. The Data Repository is the archival side and mints DOIs. Movebank has a documented REST API. Verify before relying on these values.",
   url="https://www.movebank.org"),

 dict(name="Marine Data Archive (VLIZ)", domain_scope="specific",
   domain="hydrosphere;biosphere",
   access="NA", license="NA", max_size_gb="NA",
   embargo_supported="NA", embargo_max_months="NA",
   version_control="NA", api_access="NA", doi_service="NA",
   eligibility="NA", institutions="NA",
   metadata_req="NA", data_standard_req="NA", license_req="NA",
   confidence="Low",
   notes="Every field still needs checking against VLIZ documentation. VLIZ's general DOI guidelines describe DOIs for publicly downloadable datasets and new DOIs for new versions, but do not name the Marine Data Archive, so those values are left open here. Named by one respondent for a marine pollution project.",
   url="https://www.marinedataarchive.org"),

 dict(name="ENA (EMBL-EBI)", domain_scope="specific", domain="biosphere",
   access="Open", license="NA", max_size_gb="Inf",
   embargo_supported="Yes", embargo_max_months="NA",
   version_control="Yes", api_access="Yes", doi_service="No",
   eligibility="Open", institutions="NA",
   metadata_req="INSDC submission metadata; sample and study records are required.",
   data_standard_req="INSDC formats such as FASTQ, BAM and CRAM.",
   license_req="No CC licence is applied; data are released into the public domain.",
   confidence="Medium",
   notes="Uses a confidential hold with a release date the submitter sets and can extend, rather than a fixed maximum. Records are cited by accession number (with version suffix) rather than DOI. Built for very large volumes. The route for eDNA and molecular ecology. MGnify is an EMBL-EBI analysis layer on top of ENA rather than a separate repository: metagenomic submissions go through the same ENA Webin account and MGnify then provides free taxonomic and functional analysis.",
   url="https://www.ebi.ac.uk/ena/browser/home"),

 dict(name="NCBI (SRA / GenBank)", domain_scope="specific", domain="biosphere",
   access="Open", license="NA", max_size_gb="Inf",
   embargo_supported="Yes", embargo_max_months="NA",
   version_control="Yes", api_access="Yes", doi_service="No",
   eligibility="Open", institutions="NA",
   metadata_req="BioProject and BioSample records are required alongside the data.",
   data_standard_req="INSDC formats such as FASTQ and BAM.",
   license_req="No CC licence is applied; data are released into the public domain.",
   confidence="Medium",
   notes="Same hold-until-publication model as ENA, and mirrored to it through INSDC. Cited by accession number rather than DOI; programmatic access through E-utilities. One respondent argued for international over NL or WUR repositories on visibility grounds, with the caveat of US dependency.",
   url="https://www.ncbi.nlm.nih.gov/sra"),

 dict(name="figshare", domain_scope="any", domain="environment",
   access="Open;Restricted;Closed", license=ALL_CC, max_size_gb="NA",
   embargo_supported="Yes", embargo_max_months="Inf",
   version_control="Yes", api_access="Yes", doi_service="Yes",
   eligibility="Open", institutions="NA",
   metadata_req="DataCite metadata; item type and categories are required.",
   data_standard_req="None imposed.",
   license_req="The full CC range is available; CC BY is the default for data.",
   confidence="Low",
   notes="The embargo date is set by the depositor with no stated cap. Size limits differ between the free tier and institutional instances - check which one applies to you.",
   url="https://help.figshare.com"),

 dict(name="OSF", domain_scope="any", domain="research",
   access="Open;Restricted", license=ALL_CC, max_size_gb="50",
   embargo_supported="Yes", embargo_max_months="NA",
   version_control="Yes", api_access="Yes", doi_service="Yes",
   eligibility="Open", institutions="NA",
   metadata_req="Minimal; a free-text project description.",
   data_standard_req="None imposed.",
   license_req="A licence is chosen per project from a standard list.",
   confidence="Low",
   notes="Embargo applies to registrations rather than ordinary projects, which are simply public or private, so the cap does not map cleanly onto this filter. Files are versioned automatically. Check current storage limits before relying on the size value.",
   url="https://help.osf.io"),

 dict(name="Yoda (WUR)", domain_scope="any", domain="research",
   access="Open;Restricted;Closed", license="NA", max_size_gb="NA",
   embargo_supported="NA", embargo_max_months="NA",
   version_control="Yes", api_access="NA", doi_service="Yes",
   eligibility="Members only", institutions=WUR,
   metadata_req="Configurable metadata schema per research group.",
   data_standard_req="Set by the research group.",
   license_req="Chosen at the point of publication.",
   confidence="Low",
   notes="WUR and SURF hosted, so only open to WUR-affiliated depositors. Every published data package gets a DOI automatically; published packages cannot be changed, and a new version gets its own DOI linked to the earlier one. Access is via the web portal, network disk and command-line tools; no public data API was confirmed. Named by a Marine Research respondent as their preferred future route.",
   url="https://yoda.wur.nl"),

 dict(name="Git@WUR", domain_scope="any", domain="technology",
   access="Restricted;Closed", license="CC0", max_size_gb="10",
   embargo_supported="No", embargo_max_months="NA",
   version_control="Yes", api_access="Yes", doi_service="No",
   eligibility="Members only", institutions=WUR,
   metadata_req="A repository README; no formal metadata schema.",
   data_standard_req="None.",
   license_req="Any licence, set in the repository.",
   confidence="Medium",
   notes="Version control for code rather than a data archive - no DOI service and no embargo mechanism. GitLab-based, so the GitLab API is available. Suited to scripts and configuration accompanying a dataset held elsewhere.",
   url="https://git.wur.nl"),

 # ---------------------------------------------------------------- 15-24
 dict(name="SEANOE", domain_scope="specific", domain="hydrosphere",
   access="Open", license=ALL_CC, max_size_gb="100",
   embargo_supported="Yes", embargo_max_months="24",
   version_control="Yes", api_access="NA", doi_service="Yes",
   eligibility="Open", institutions="NA",
   metadata_req="Descriptive metadata required. Every deposit is checked before publication and may be refused if it fails quality or scope criteria.",
   data_standard_req="Perennial non-proprietary formats requested (CSV rather than Excel). SEANOE advises on format choice.",
   license_req="A Creative Commons licence is selected by the depositor; most datasets are CC BY.",
   confidence="High",
   notes="100 GB per record and free. DOI issued within 24 hours for well-described datasets. Embargo capped at two years; during the embargo the DOI is live and the landing page public, only the files are withheld. Deposits are permanent - published data cannot be deleted, only updated. Operated by Ifremer/SISMER; records duplicate to the EMODnet Data Ingestion portal.",
   url="https://www.seanoe.org/html/about.htm"),

 dict(name="Environmental Information Data Centre (EIDC)", domain_scope="specific", domain="biosphere",
   access="Open;Restricted", license="Other", max_size_gb="NA",
   embargo_supported="Yes", embargo_max_months="24",
   version_control="Yes", api_access="NA", doi_service="Yes",
   eligibility="Open", institutions="NA",
   metadata_req="Metadata supplied via a deposit request form and reviewed by EIDC staff. Supporting documentation expected.",
   data_standard_req="Non-proprietary formats required - CSV rather than Excel. A published list of acceptable formats applies.",
   license_req="The UK Open Government Licence, not a Creative Commons licence. The OGL is CC BY-compatible in practice but is not one of the CC codes. Bespoke licences apply to some third-party datasets.",
   confidence="High",
   notes="CoreTrustSeal certified and a World Data System member. DOIs for the majority of deposits; a major change (added or altered data) gets a new DOI. No published size cap - volume is agreed case by case in a Service Agreement. The two-year embargo guidance runs from the END OF DATA COLLECTION, not from deposit. Any researcher in terrestrial or freshwater sciences may deposit, though NERC-funded work is given priority.",
   url="https://eidc.ac.uk/deposit"),

 dict(name="ICOS Carbon Portal", domain_scope="specific", domain="atmosphere",
   access="Open;Restricted", license="CC BY", max_size_gb="NA",
   embargo_supported="Yes", embargo_max_months="Inf",
   version_control="Yes", api_access="Yes", doi_service="Yes",
   eligibility="Members only", institutions="ICOS ERIC",
   metadata_req="Rich structured metadata via the ICOS metadata service. The metadata itself is released under CC0.",
   data_standard_req="Standardised ICOS data levels and formats. Ecosystem products follow the FLUXNET archive format.",
   license_req="Fixed at CC BY 4.0 for all ICOS data. The depositor does not choose a licence.",
   confidence="High",
   notes="Uploads are normally performed only by trusted collaborators from the ICOS Thematic Centres or by Carbon Portal staff, so this is not a self-deposit route for an unaffiliated ecologist. Embargo is an optional moratorium timestamp with no documented maximum. Newer data objects supersede older ones while the old versions stay available. SPARQL endpoint and Python library for programmatic access. Issues DOIs and Handle PIDs.",
   url="https://www.icos-cp.eu/data-services/about-data-portal/data-licence"),

 dict(name="B2SHARE (EUDAT)", domain_scope="any", domain="environment",
   access="Open;Restricted", license=ALL_CC, max_size_gb="20",
   embargo_supported="Yes", embargo_max_months="Inf",
   version_control="Yes", api_access="Yes", doi_service="Yes",
   eligibility="Open", institutions="NA",
   metadata_req="Metadata is mandatory and enforced at deposit; the schema varies by the community you deposit under. Title, description and access status are required.",
   data_standard_req="All formats accepted provided the data has a scientific purpose.",
   license_req="Licence is optional at deposit but a built-in wizard helps select one. Any licence may be applied.",
   confidence="High",
   notes="10 GB per file and 20 GB per record on the basic free service; a Premium tier handles larger datasets on request. Open Access is the default. The embargo date is an optional field with no documented cap. Open to institutional researchers and to individual citizen scientists. Issues a DOI plus a B2HANDLE PID; REST API. Free.",
   url="https://eudat.eu/services/userdoc/b2share-faq"),

 dict(name="Ocean Biodiversity Information System (OBIS)", domain_scope="specific", domain="biosphere",
   access="Open", license="CC0;CC BY;CC BY-NC", max_size_gb="NA",
   embargo_supported="No", embargo_max_months="NA",
   version_control="Yes", api_access="Yes", doi_service="No",
   eligibility="Open", institutions="NA",
   metadata_req="Ecological Metadata Language (EML) required. Darwin Core required for the occurrence records themselves.",
   data_standard_req="Darwin Core is mandatory. Data must pass node quality assurance and validation before it enters OBIS.",
   license_req="Only CC0 (preferred), CC BY and CC BY-NC are accepted. CC BY-SA and CC BY-NC-SA are not.",
   confidence="High",
   notes="OBIS offers no embargo, but providers are expected to censor or generalise sensitive records before publishing - the standard route for rare or threatened species localities. Publishing goes via a regional or thematic node's IPT rather than direct self-deposit; the IPT keeps versions. OBIS itself does not mint DOIs, but one can be reserved through the IPT if the node has a DataCite account set up.",
   url="https://manual.obis.org/policy.html"),

 dict(name="Centre for Environmental Data Analysis (CEDA)", domain_scope="specific", domain="atmosphere",
   access="Open;Restricted", license="Other", max_size_gb="10000",
   embargo_supported="Yes", embargo_max_months="24",
   version_control="NA", api_access="Yes", doi_service="Yes",
   eligibility="Open", institutions="NA",
   metadata_req="Catalogue metadata record prepared with CEDA. Dataset descriptions are made freely available for any purpose.",
   data_standard_req="Formats agreed with CEDA at deposit. The archive is selective and decides what it accepts.",
   license_req="Most datasets carry the UK Open Government Licence rather than a Creative Commons licence. Generic licences exist for embargoed and restricted data (RUGL, RUNCGL).",
   confidence="High",
   notes="Projects can archive up to 10 TB. For NERC-funded projects the embargo maximum is two years from data production. Three access tiers: public, registered-user and restricted. OpenSearch API for the archive. Scope is primarily NERC-funded atmospheric science and Earth observation; small volumes from non-NERC projects are accepted. Part of the NERC Environmental Data Service alongside EIDC and BODC.",
   url="https://help.ceda.ac.uk/article/4661-depositing-data-faqs"),

 dict(name="British Oceanographic Data Centre (BODC)", domain_scope="specific", domain="hydrosphere",
   access="Open;Restricted", license="Other", max_size_gb="NA",
   embargo_supported="Yes", embargo_max_months="24",
   version_control="Yes", api_access="NA", doi_service="Yes",
   eligibility="Open", institutions="NA",
   metadata_req="A BODC metadata submission template must be completed for every dataset, plus a comprehensive abstract describing the data.",
   data_standard_req="Marine science formats agreed with BODC. Submitted data is made discoverable via a European Directory of Marine Environmental Data (EDMED) record.",
   license_req="Follows NERC Environmental Data Service practice; most resources carry the UK Open Government Licence rather than a Creative Commons licence.",
   confidence="Medium",
   notes="UK National Oceanographic Data Centre since 1969. Standard embargo is two years from the date the data were COLLECTED, waivable on request. A DOI is not automatic - DOIs are issued through the Published Data Library for selected datasets only. Published datasets are never changed: a new version gets a new DOI and the landing page shows the full version history.",
   url="https://www.bodc.ac.uk/submit_data/data_planning/later_in_your_project/"),

 dict(name="World Data Center for Climate (WDCC)", domain_scope="specific", domain="atmosphere",
   access="Open;Restricted", license="NA", max_size_gb="NA",
   embargo_supported="NA", embargo_max_months="NA",
   version_control="Yes", api_access="NA", doi_service="Yes",
   eligibility="Open", institutions="NA",
   metadata_req="Highly standardised metadata via the MetaXA submission portal. WDCC operates a published minimal metadata standard that submissions must meet.",
   data_standard_req="Open-source formats required; Zarr is explicitly not supported. Structure and format are agreed with the DKRZ Data Management team before submission.",
   license_req="Agreed case by case in the Submission Agreement, alongside authorship and ownership. No fixed licence set is published.",
   confidence="Medium",
   notes="CoreTrustSeal certified and a World Data System member; issued the first ever DataCite dataset DOI in 2004. Curation guaranteed for at least 10 years. Deposit is a mediated multi-step process, not self-service, so allow lead time. Size is negotiated rather than capped. No embargo mechanism or public data API was found in the documentation.",
   url="https://www.wdc-climate.de/ui/info?site=guide_wdcc"),

 dict(name="GFZ Data Services", domain_scope="any", domain="lithosphere",
   access="Open;Restricted", license=ALL_CC, max_size_gb="NA",
   embargo_supported="Yes", embargo_max_months="36",
   version_control="NA", api_access="Yes", doi_service="Yes",
   eligibility="Open", institutions="NA",
   metadata_req="Structured metadata via the GFZ Metadata Editor following the DataCite schema. Metadata itself is placed in the public domain.",
   data_standard_req="Domain-appropriate open formats. Consultation is offered by domain scientists and IT specialists.",
   license_req="Open international licences recommended: Creative Commons for data, open-source licences for software. The licence is displayed on each DOI landing page.",
   confidence="Medium",
   notes="Assigning DOIs to datasets since 2004. Handles both large monitoring-network datasets and small long-tail datasets. GFZ guidelines allow an embargo of up to three years after project completion. OAI-PMH interface for metadata harvesting. No published size cap.",
   url="https://dataservices.gfz-potsdam.de/portal/about.html"),

 dict(name="ISRIC World Soil Information (WoSIS)", domain_scope="specific", domain="lithosphere",
   access="Open;Restricted", license="CC BY;CC BY-NC", max_size_gb="NA",
   embargo_supported="Yes", embargo_max_months="NA",
   version_control="NA", api_access="Yes", doi_service="Yes",
   eligibility="Open", institutions="NA",
   metadata_req="DataCite Metadata Schema and ISO 19115. Datasets are assessed for data and metadata quality before inclusion in WoSIS.",
   data_standard_req="Soil profile data is standardised into the WoSIS structure by ISRIC after submission.",
   license_req="Only CC BY and CC BY-NC are used for datasets; derived products such as SoilGrids are released CC BY 4.0. Your existing licence is respected.",
   confidence="Medium",
   notes="World Data Centre for Soils and a World Data System member, founded 1966. You retain ownership and data is never shared with third parties without consent. Embargo is offered but no maximum is published. Sources conflict on who may deposit (ISRIC invites contributions broadly; re3data records upload as restricted) - worth confirming. Web services give programmatic access to WoSIS and SoilGrids.",
   url="https://www.isric.org/about/data-policy"),
]

# ------------------------------------------------------------------ licences
NR, CR, SA, NC, CU = ("No restrictions", "Credit required", "Share alike",
                      "Non-commercial only", "Custom")
OPEN4 = [NR, CR, SA, NC]
CC_NAMES = "CC0;CC BY;CC BY-SA;CC BY-NC;CC BY-NC-SA"

# name -> (licence names shown on the card, licence terms used by the filter)
LICENSES = {
    "Zenodo":             (CC_NAMES, OPEN4),
    "4TU.ResearchData":   (CC_NAMES, OPEN4),
    "DANS Data Stations": (CC_NAMES + ";DANS Licence (restricted files)", OPEN4 + [CU]),
    "Dryad":              ("CC0", [NR]),
    "PANGAEA":            ("CC0;CC BY;CC BY-NC", [NR, CR, NC]),
    "GBIF":               ("CC0;CC BY;CC BY-NC", [NR, CR, NC]),
    "Movebank":           ("CC0", [NR]),
    "Marine Data Archive (VLIZ)": ("NA", None),
    "ENA (EMBL-EBI)":     ("Public domain (INSDC)", [NR]),
    "NCBI (SRA / GenBank)": ("Public domain (INSDC)", [NR]),
    "figshare":           (CC_NAMES, OPEN4),
    "OSF":                (CC_NAMES + ";other standard licences", OPEN4),
    "Yoda (WUR)":         ("NA", None),
    "Git@WUR":            ("Any licence, e.g. MIT;Apache 2.0;GPL;CC", OPEN4),
    "SEANOE":             (CC_NAMES, OPEN4),
    "Environmental Information Data Centre (EIDC)":
                          ("UK Open Government Licence;bespoke licences for some third-party data", [CR, CU]),
    "ICOS Carbon Portal": ("CC BY 4.0", [CR]),
    "B2SHARE (EUDAT)":    (CC_NAMES + ";any other licence", OPEN4 + [CU]),
    "Ocean Biodiversity Information System (OBIS)": ("CC0;CC BY;CC BY-NC", [NR, CR, NC]),
    "Centre for Environmental Data Analysis (CEDA)":
                          ("UK Open Government Licence;restricted-use licences (RUGL, RUNCGL)", [CR, CU]),
    "British Oceanographic Data Centre (BODC)": ("UK Open Government Licence", [CR]),
    "World Data Center for Climate (WDCC)": ("Agreed per submission", [CU]),
    "GFZ Data Services":  (CC_NAMES + ";open-source software licences", OPEN4),
    "ISRIC World Soil Information (WoSIS)": ("CC BY;CC BY-NC", [CR, NC]),
}

for row in ROWS:
    names, terms = LICENSES[row["name"]]
    row["license"] = names
    row["license_terms"] = "NA" if terms is None else ";".join(terms)

assert set(LICENSES) == {r["name"] for r in ROWS}

AFFILIATION = {"Members only": "Yes", "Open": "No", "Open with fees": "No"}
for row in ROWS:
    row["affiliation_required"] = AFFILIATION.get(row["eligibility"], "NA")

COMMUNITY_COLUMNS = ["name", "field", "domain", "how_to_contribute", "notes", "url"]

COMMUNITY_ROWS = [
 dict(name="SPI-Birds", field="Birds", domain="biosphere",
   how_to_contribute="Join the network and share your long-term individual-based breeding data from nest-box or similar studies; SPI-Birds standardises it to a common format.",
   notes="Data owners keep control: users request access and the owner approves each request. Not an archive - deposit a citable copy elsewhere as well.",
   url="https://spibirds.org"),
 dict(name="EUROMAMMALS", field="Mammals", domain="biosphere",
   how_to_contribute="Join as a partner and contribute GPS-tracking and related data on European mammals (e.g. roe deer, red deer, wild boar, large carnivores).",
   notes="Collaborative, data-owner-driven project: data is shared within the network under its rules, with co-authorship for contributors.",
   url="https://www.euromammals.org"),
 dict(name="European Butterfly Monitoring Scheme (eBMS)", field="Insects", domain="biosphere",
   how_to_contribute="Contribute butterfly transect counts through a national Butterfly Monitoring Scheme (in the Netherlands via De Vlinderstichting) or the eBMS app.",
   notes="Pools national monitoring schemes into European trend indicators. Access to the pooled data goes through a data request.",
   url="https://butterfly-monitoring.net"),
 dict(name="TRY Plant Trait Database", field="Vegetation", domain="biosphere",
   how_to_contribute="Contribute plant trait datasets; contributors are credited and gain access to the pooled database.",
   notes="Most data is available on request with a short project proposal; part of it is open access.",
   url="https://www.try-db.org"),
 dict(name="sPlot", field="Vegetation", domain="biosphere",
   how_to_contribute="Vegetation-plot databases are integrated into sPlot via data providers, often through EVA or national databases.",
   notes="Global vegetation-plot database hosted by iDiv. Access via project proposals to the consortium.",
   url="https://www.idiv.de/en/splot.html"),
 dict(name="European Vegetation Archive (EVA)", field="Vegetation", domain="biosphere",
   how_to_contribute="NA",
   notes="Standard resource for European vegetation-plot data. Contribution route, access rules and citation practice are unverified - check before relying on this entry.",
   url="http://euroveg.org"),
]


def write(path, columns, rows):
    with open(path, "w", newline="", encoding="utf-8") as fh:
        w = csv.DictWriter(fh, fieldnames=columns, quoting=csv.QUOTE_MINIMAL)
        w.writeheader()
        for row in rows:
            assert set(row) == set(columns), (row["name"], set(row) ^ set(columns))
            w.writerow(row)
    print(f"wrote {len(rows)} rows to {os.path.relpath(path, HERE)}")


if __name__ == "__main__":
    os.makedirs(DATA, exist_ok=True)
    write(os.path.join(DATA, "repositories.csv"), COLUMNS, ROWS)
    write(os.path.join(DATA, "community_databases.csv"), COMMUNITY_COLUMNS, COMMUNITY_ROWS)
