function docdistances()
    rrh = lower(importdata('RedRidingHood.txt'));
    rrh = strsplit(rrh{:});
    corpus = string(rrh);
    pp = lower(importdata('PrincessPea.txt'));
    pp = strsplit(pp{:});
    corpus = [corpus, string(pp)];
    cin = lower(importdata('Cinderella.txt'));
    cin = strsplit(cin{:});
    corpus = [corpus, string(cin)];
    c1 = lower(importdata('CAFA1.txt'));
    c1 = strsplit(c1{:});
    corpus = [corpus, string(c1)];
    c2 = lower(importdata('CAFA2.txt'));
    c2 = strsplit(c2{:});
    corpus = [corpus, string(c2)];
    c3 = lower(importdata('CAFA3.txt'));
    c3 = strsplit(c3{:});
    corpus = [corpus, string(c3)];
    corpus = unique(corpus);
    stories = {string(rrh), string(pp), string(cin), string(c1), string(c2), string(c3)};
    
    %Finding the df
    docwords = cellfun(@unique, stories, 'UniformOutput', false);
    df = cell2mat(cellfun(@(x)ismember(corpus, x), docwords, 'UniformOutput', false));
    df = reshape(df, length(corpus), length(docwords))';
    df = sum(df);
    idf = 6 ./ df;
    
    %Finding the tf
    tf = cellfun(@(x) arrayfun(@(y) length(x(strcmp(y,x))), corpus), stories, 'UniformOutput', false);
    
    %Norms of the vectors
    %vecs = cellfun(@(t) [t;idf]', tf, 'UniformOutput', false);
    %norms = cellfun(@(v) vecnorm(v'), vecs, 'UniformOutput', false);
    vecs = cellfun(@(t) [t .* idf]', tf, 'UniformOutput', false);
    matvecs = cell2mat(vecs);
    norms = cellfun(@(d) norm(d), vecs);
    norms = norms' * norms;
    dots = matvecs' * matvecs;
    cosdist = 1 - (dots ./ norms);
    
    imagesc(cosdist)
    colormap('summer')
    
end